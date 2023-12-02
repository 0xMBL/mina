wget https://raw.githubusercontent.com/MinaProtocol/mina/rampup/src/app/archive/create_schema.sql -O rampup_create_schema.sql
wget https://raw.githubusercontent.com/MinaProtocol/mina/rampup/src/app/archive/zkapp_tables.sql

docker stop postgres
docker rm postgres
docker run --name postgres -p 5555:5432 -e POSTGRES_USER=postgres -e POSTGRES_PASSWORD=postgres -e POSTGRES_DB=archive -d postgres

END_GLOBAL_SLOT=300

READY=""
while [[ $READY != *"accepting connections"* ]]; do
    READY=$(docker container exec -it  postgres pg_isready -U postgres)

    echo $READY
    sleep 5
done

wget -c https://storage.googleapis.com/mina-archive-dumps/mainnet-archive-dump-2023-11-02_0000.sql.tar.gz -O - | tar -xz

PGPASSWORD=postgres psql -h localhost -p 5555 -U postgres archive < mainnet-archive-dump-2023-11-02_0000.sql
PGPASSWORD=postgres psql -h localhost -p 5555 -U postgres -c 'CREATE DATABASE migrated;'
PGPASSWORD=postgres psql -h localhost -p 5555 -U postgres -d migrated -a -f rampup_create_schema.sql

dune exec src/app/berkeley_migration/berkeley_migration.exe -- --config-file genesis_ledgers/mainnet.json --mainnet-archive-uri postgres://postgres:postgres@localhost:5555/archive_balances_migrated --migrated-archive-uri postgres://postgres:postgres@localhost:5555/migrated --mainnet-blocks-bucket "mina_network_block_data" --end-global-slot $END_GLOBAL_SLOT --batch-size 100

GENESIS_LEDGER=$(cat "genesis_ledgers/mainnet.json" | jq '(.ledger)' )

echo "{ \"start_slot_since_genesis\":1, \"genesis_ledger\":${GENESIS_LEDGER} }" > genesis_enhanced.json

dune exec src/app/berkeley_account_tables/berkeley_account_tables.exe -- --archive-uri postgres://postgres:postgres@localhost:5555/migrated --input-file genesis_enhanced.json --output-file output_ledger.json

dune exec src/app/berkeley_migration_data_check/berkeley_migration_data_check.exe -- --archive-uri postgres://postgres:postgres@localhost:5555/archive_balances_migrated --migrated-uri postgres://postgres:postgres@localhost:5555/migrated --end-global-slot-from 100 --end-global-slot-to $END_GLOBAL_SLOT --genesis-ledger-file genesis_ledgers/mainnet.json --genesis-accounts-test-count 1000
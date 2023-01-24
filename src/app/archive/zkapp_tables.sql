/* zkapp_tables.sql -- support tables for Zkapp commands */

/* Several of the tables below support the following convention, related
   to NULL values.

   In OCaml, some Zkapp-related types use the constructors Check, which takes a value,
   and Ignore, which is nullary. In columns following the convention, a NULL means Ignore, while
   non-NULL means Check.

   Similarly, in OCaml, there are the constructors Set, which takes a value, and
   Keep, which is nullary. NULL means Keep, and non-NULL means Set.

   The tables that follow this convention have a comment "NULL convention".
*/

/* the string representation of an algebraic field */
CREATE TABLE zkapp_state_data
( id                       serial           PRIMARY KEY
, field                    text             NOT NULL UNIQUE
);

/* Variable-width arrays of algebraic fields, given as
   id's from zkapp_state_data

   Postgresql does not allow enforcing that the array elements are
   foreign keys

   The elements of the array are NOT NULL (not enforced by Postgresql)

*/
CREATE TABLE zkapp_state_data_array
( id                       serial  PRIMARY KEY
, element_ids              int[]   NOT NULL
);

/* Fixed-width arrays of algebraic fields, given as id's from
   zkapp_state_data

   Any element of the array may be NULL, per the NULL convention
*/
CREATE TABLE zkapp_states_nullable
( id                       serial           PRIMARY KEY
, element0                 int              REFERENCES zkapp_state_data(id)
, element1                 int		    REFERENCES zkapp_state_data(id)
, element2                 int		    REFERENCES zkapp_state_data(id)
, element3                 int		    REFERENCES zkapp_state_data(id)
, element4                 int		    REFERENCES zkapp_state_data(id)
, element5                 int		    REFERENCES zkapp_state_data(id)
, element6                 int		    REFERENCES zkapp_state_data(id)
, element7                 int		    REFERENCES zkapp_state_data(id)
);

/* like zkapp_states_nullable, but elements are not NULL */
CREATE TABLE zkapp_states
( id                       serial           PRIMARY KEY
, element0                 int              NOT NULL REFERENCES zkapp_state_data(id)
, element1                 int              NOT NULL REFERENCES zkapp_state_data(id)
, element2                 int              NOT NULL REFERENCES zkapp_state_data(id)
, element3                 int              NOT NULL REFERENCES zkapp_state_data(id)
, element4                 int              NOT NULL REFERENCES zkapp_state_data(id)
, element5                 int              NOT NULL REFERENCES zkapp_state_data(id)
, element6                 int              NOT NULL REFERENCES zkapp_state_data(id)
, element7                 int              NOT NULL REFERENCES zkapp_state_data(id)
);

/* like zkapp_states, but for sequence states */
CREATE TABLE zkapp_sequence_states
( id                       serial           PRIMARY KEY
, element0                 int              NOT NULL REFERENCES zkapp_state_data(id)
, element1                 int              NOT NULL REFERENCES zkapp_state_data(id)
, element2                 int              NOT NULL REFERENCES zkapp_state_data(id)
, element3                 int              NOT NULL REFERENCES zkapp_state_data(id)
, element4                 int              NOT NULL REFERENCES zkapp_state_data(id)
);

/* the element_ids are non-NULL, and refer to zkapp_state_data_array
   that is, they represent a list of arrays of field elements
*/
CREATE TABLE zkapp_events
( id                       serial           PRIMARY KEY
, element_ids              int[]            NOT NULL
);

CREATE TABLE zkapp_verification_keys
( id                       serial           PRIMARY KEY
, verification_key         text             NOT NULL UNIQUE
, hash                     text             NOT NULL UNIQUE
);

CREATE TYPE zkapp_auth_required_type AS ENUM ('none', 'either', 'proof', 'signature', 'both', 'impossible');

CREATE TABLE zkapp_permissions
( id                       serial                PRIMARY KEY
, edit_state               zkapp_auth_required_type    NOT NULL
, send                     zkapp_auth_required_type    NOT NULL
, receive                  zkapp_auth_required_type    NOT NULL
, access                   zkapp_auth_required_type    NOT NULL
, set_delegate             zkapp_auth_required_type    NOT NULL
, set_permissions          zkapp_auth_required_type    NOT NULL
, set_verification_key     zkapp_auth_required_type    NOT NULL
, set_zkapp_uri            zkapp_auth_required_type    NOT NULL
, edit_sequence_state      zkapp_auth_required_type    NOT NULL
, set_token_symbol         zkapp_auth_required_type    NOT NULL
, increment_nonce          zkapp_auth_required_type    NOT NULL
, set_voting_for           zkapp_auth_required_type    NOT NULL
);

CREATE TABLE zkapp_timing_info
( id                       serial  PRIMARY KEY
, initial_minimum_balance  text    NOT NULL
, cliff_time               bigint  NOT NULL
, cliff_amount             text    NOT NULL
, vesting_period           bigint  NOT NULL
, vesting_increment        text    NOT NULL
);

CREATE TABLE zkapp_uris
( id                 serial  PRIMARY KEY
, value              text    NOT NULL UNIQUE
);

/* NULL convention */
CREATE TABLE zkapp_updates
( id                       serial           PRIMARY KEY
, app_state_id             int              NOT NULL REFERENCES zkapp_states_nullable(id)
, delegate_id              int              REFERENCES public_keys(id)
, verification_key_id      int              REFERENCES zkapp_verification_keys(id)
, permissions_id           int              REFERENCES zkapp_permissions(id)
, zkapp_uri_id             int              REFERENCES zkapp_uris(id)
, token_symbol_id          int              REFERENCES token_symbols(id)
, timing_id                int              REFERENCES zkapp_timing_info(id)
, voting_for_id            int              REFERENCES voting_for(id)
);

CREATE TABLE zkapp_balance_bounds
( id                       serial           PRIMARY KEY
, balance_lower_bound      text             NOT NULL
, balance_upper_bound      text             NOT NULL
);

CREATE TABLE zkapp_nonce_bounds
( id                       serial           PRIMARY KEY
, nonce_lower_bound        bigint           NOT NULL
, nonce_upper_bound        bigint           NOT NULL
);

CREATE TYPE zkapp_precondition_type AS ENUM ('full', 'nonce', 'accept');

/* NULL convention */
CREATE TABLE zkapp_account_precondition_values
( id                       serial                 PRIMARY KEY
, balance_id               int                    REFERENCES zkapp_balance_bounds(id)
, nonce_id                 int                    REFERENCES zkapp_nonce_bounds(id)
, receipt_chain_hash       text
, delegate_id              int                    REFERENCES public_keys(id)
, state_id                 int        NOT NULL    REFERENCES zkapp_states_nullable(id)
, sequence_state_id        int                    REFERENCES zkapp_state_data(id)
, proved_state             boolean
, is_new                   boolean
);

/* invariants: precondition_account id is not NULL iff kind is 'full'
               nonce is not NULL iff kind is 'nonce'
*/
CREATE TABLE zkapp_account_precondition
( id                       serial                            PRIMARY KEY
, kind                     zkapp_precondition_type           NOT NULL
, account_precondition_values_id  int                               REFERENCES zkapp_account_precondition_values(id)
, nonce                    bigint
);

CREATE TABLE zkapp_accounts
( id                   serial  PRIMARY KEY
, app_state_id         int     NOT NULL  REFERENCES zkapp_states(id)
, verification_key_id  int               REFERENCES zkapp_verification_keys(id)
, zkapp_version        bigint  NOT NULL
, sequence_state_id    int     NOT NULL  REFERENCES zkapp_sequence_states(id)
, last_sequence_slot   bigint  NOT NULL
, proved_state         bool    NOT NULL
, zkapp_uri_id         int     NOT NULL  REFERENCES zkapp_uris(id)
);

CREATE TABLE zkapp_token_id_bounds
( id                       serial           PRIMARY KEY
, token_id_lower_bound     text             NOT NULL
, token_id_upper_bound     text             NOT NULL
);

CREATE TABLE zkapp_length_bounds
( id                       serial          PRIMARY KEY
, length_lower_bound       bigint          NOT NULL
, length_upper_bound       bigint          NOT NULL
);

CREATE TABLE zkapp_amount_bounds
( id                       serial          PRIMARY KEY
, amount_lower_bound       text            NOT NULL
, amount_upper_bound       text            NOT NULL
);

CREATE TABLE zkapp_global_slot_bounds
( id                       serial          PRIMARY KEY
, global_slot_lower_bound  bigint          NOT NULL
, global_slot_upper_bound  bigint          NOT NULL
);

/* NULL convention */
CREATE TABLE zkapp_epoch_ledger
( id                       serial          PRIMARY KEY
, hash_id                  int             REFERENCES snarked_ledger_hashes(id)
, total_currency_id        int             REFERENCES zkapp_amount_bounds(id)
);

/* NULL convention */
CREATE TABLE zkapp_epoch_data
( id                       serial          PRIMARY KEY
, epoch_ledger_id          int             REFERENCES zkapp_epoch_ledger(id)
, epoch_seed               text
, start_checkpoint         text
, lock_checkpoint          text
, epoch_length_id          int             REFERENCES zkapp_length_bounds(id)
);

/* NULL convention */
CREATE TABLE zkapp_network_precondition
( id                               serial                         NOT NULL PRIMARY KEY
, snarked_ledger_hash_id           int                            REFERENCES snarked_ledger_hashes(id)
, blockchain_length_id             int                            REFERENCES zkapp_length_bounds(id)
, min_window_density_id            int                            REFERENCES zkapp_length_bounds(id)
/* omitting 'last_vrf_output' for now, it's the unit value in OCaml */
, total_currency_id                int                            REFERENCES zkapp_amount_bounds(id)
, global_slot_since_genesis        int                            REFERENCES zkapp_global_slot_bounds(id)
, staking_epoch_data_id            int                            REFERENCES zkapp_epoch_data(id)
, next_epoch_data_id               int                            REFERENCES zkapp_epoch_data(id)
);

/* events_ids and actions_ids indicate a list of ids in
   zkapp_state_data_array.
*/
CREATE TABLE zkapp_fee_payer_body
( id                                    serial    PRIMARY KEY
, account_identifier_id                 int       NOT NULL REFERENCES account_identifiers(id)
, fee                                   text      NOT NULL
, valid_until                           bigint
, nonce                                 bigint    NOT NULL
);

CREATE TYPE call_type AS ENUM ('call', 'delegate_call');

CREATE TYPE authorization_kind_type AS ENUM ('None_given', 'Signature', 'Proof');

/* events_ids and actions_ids indicate a list of ids in
   zkapp_state_data_array.
*/
CREATE TABLE zkapp_account_update_body
( id                                    serial          PRIMARY KEY
, account_identifier_id                 int             NOT NULL  REFERENCES account_identifiers(id)
, update_id                             int             NOT NULL  REFERENCES zkapp_updates(id)
, balance_change                        text            NOT NULL
, increment_nonce                       boolean         NOT NULL
, events_id                             int             NOT NULL  REFERENCES zkapp_events(id)
, actions_id                            int             NOT NULL  REFERENCES zkapp_events(id)
, call_data_id                          int             NOT NULL  REFERENCES zkapp_state_data(id)
, call_depth                            int             NOT NULL
, zkapp_network_precondition_id         int             NOT NULL  REFERENCES zkapp_network_precondition(id)
, zkapp_account_precondition_id         int             NOT NULL  REFERENCES zkapp_account_precondition(id)
, zkapp_valid_while_precondition_id     int                       REFERENCES zkapp_global_slot_bounds(id)
, use_full_commitment                   boolean         NOT NULL
, implicit_account_creation_fee         boolean         NOT NULL
, call_type                             call_type       NOT NULL
, authorization_kind                    authorization_kind_type NOT NULL
);

/* possible future enhancement: add NULLable authorization column for proofs and signatures */
CREATE TABLE zkapp_account_update
( id                       serial                          PRIMARY KEY
, body_id                  int                             NOT NULL REFERENCES zkapp_account_update_body(id)
);

/* a list of of failures for an account update in a zkApp
   the index is the index into the `account_updates`
*/
CREATE TABLE zkapp_account_update_failures
( id       serial    PRIMARY KEY
, index    int       NOT NULL
, failures text[]    NOT NULL
);
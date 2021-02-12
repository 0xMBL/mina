# Global Vars

variable "environment" {
  description = "The environment the service is running in"
  type        = string
  default     = "dev"
}

variable "testnet" {
  description = "The testnet that this daemon is connected to"
  type        = string
}

variable "ecs_cluster_id" {
  description = "The ECS Cluster this service should be deployed to"
  type        = string
}

# graphql-proxy VARIABLES
variable "proxy_container_version" {
  description = "The version of the container to be used when deploying the graphql-proxy Service"
  type        = string
}

variable "mina_graphql_host" {
  description = "The hostname of the Mina GraphQL Endpoint"
  type        = string
  default     = "localhost"
}

variable "mina_graphql_port" {
  description = "The port the Mina GraphQL Endpoint is listening on"
  type        = string
  default     = "3085"
}

variable "proxy_external_port" {
  description = "The port the GraphQL Proxy is listening on"
  type        = string
  default     = "3000"
}

# DAEMON VARIABLES

variable "mina_container_version" {
  description = "The version of the container to be used when deploying the Mina Daemon"
  type        = string
}

variable "mina_wallet_keys" {
  description = "A space-delimited list of AWS Secrets Manager secret IDs"
  type        = string
}

variable "aws_access_key" {
  description = "An Access Key granting read-only access to Testnet Secrets"
  type        = string
}

variable "aws_secret_key" {
  description = "The corresponding AWS Secret Key"
  type        = string
}

variable "aws_default_region" {
  description = "The region that the secrets are stored in"
  type        = string
}

variable "mina_peer" {
  description = "The initial peer to start the Daemon with"
  type        = string
}

variable "mina_rest_port" {
  description = "The port that the GraphQL server will listen on"
  type        = string
  default     = "3085"
}

variable "mina_discovery_port" {
  description = "The port that the daemon will listen for RPC connections"
  type        = string
  default = "10102"
}

variable "mina_external_port" {
  description = "The port that the daemon will listen for RPC connections"
  type        = string
  default       = "10101"
}

variable "mina_metrics_port" {
  description = "The port that the daemon will expose prometheus metrics on"
  type        = string
  default       = "10000"
}

variable "mina_client_port" {
  description = "The port that the daemon will expose prometheus metrics on"
  type        = string
  default       = "10103"
}

variable "mina_privkey_pass" {
  description = "The password for the installed keys"
  type        = string
}

variable "mina_archive_node" {
  description = "Should this be run as an archive node (set if yes, unset if not)"
  type        = string
  default = "false"
}
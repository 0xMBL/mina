locals {
  service_name = "graphql-proxy-${var.testnet}-${var.environment}"
}


resource "aws_cloudwatch_log_group" "graphql-proxy" {
  name              = local.service_name
  retention_in_days = 1
}

data "template_file" "container_definition" {
  template = "${file("${path.module}/templates/container-definition.json.tpl")}"

  vars = {
    log_group = local.service_name
    region    = "us-west-2"
    # graphql-proxy Vars
    proxy_container_version = var.proxy_container_version
    mina_graphql_host = var.mina_graphql_host
    mina_graphql_port = var.mina_rest_port
    proxy_external_port = var.proxy_external_port
    # Daemon Vars
    mina_container_version = var.mina_container_version
    mina_wallet_keys   = var.mina_wallet_keys
    aws_access_key     = var.aws_access_key
    aws_secret_key     = var.aws_secret_key
    aws_default_region = var.aws_default_region
    mina_peer          = var.mina_peer
    mina_rest_port     = var.mina_rest_port
    mina_discovery_port = var.mina_discovery_port
    mina_external_port = var.mina_external_port
    mina_metrics_port  = var.mina_metrics_port
    mina_privkey_pass  = var.mina_privkey_pass
    mina_testnet  = var.testnet
    mina_archive_node = var.mina_archive_node
    mina_client_port = var.mina_client_port
  }
}

resource "aws_ecs_task_definition" "graphql-proxy" {
  family = local.service_name
  network_mode = "host"
  container_definitions = data.template_file.container_definition.rendered

  volume {
    name = "archive-node-storage"

    docker_volume_configuration {
      scope         = "shared"
      autoprovision = "true"
    }
  }
}

resource "aws_ecs_service" "graphql-proxy" {
  name            = local.service_name
  cluster         = var.ecs_cluster_id
  task_definition = aws_ecs_task_definition.graphql-proxy.arn

  desired_count = 1

  deployment_maximum_percent         = 100
  deployment_minimum_healthy_percent = 0
}
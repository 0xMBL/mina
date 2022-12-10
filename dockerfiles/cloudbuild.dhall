let Schema = ./cloudbuild-schema.dhall

let Map =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/master/Prelude/Map/package.dhall

let List/concatMap =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/master/Prelude/List/concatMap

let Optional/map =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/master/Prelude/Optional/map

let Text/concatSep =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/master/Prelude/Text/concatSep

let List/map =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/master/Prelude/List/map

let escapeShellArg = λ(arg : Text) → "'" ++ Text/replace "'" "\\'" arg ++ "'"

let DebCodename =
      < bionic
      | focal
      | impish
      | jammy
      | stretch
      | buster
      | bullseye
      | sid
      | bookworm
      >

let debInfo
    : DebCodename → { debCodename : Text, image : Text }
    = λ(codename : DebCodename) →
        let text =
              merge
                { bionic = "bionic"
                , focal = "focal"
                , impish = "impish"
                , jammy = "jammy"
                , stretch = "stretch"
                , buster = "buster"
                , bullseye = "bullseye"
                , sid = "sid"
                , bookworm = "bookworm"
                }
                codename

        in    { debCodename = text }
            ⫽ { image =
                      merge
                        { bionic = "ubuntu"
                        , focal = "ubuntu"
                        , impish = "ubuntu"
                        , jammy = "ubuntu"
                        , stretch = "debian"
                        , buster = "debian"
                        , bullseye = "debian"
                        , sid = "debian"
                        , bookworm = "debian"
                        }
                        codename
                  ++  ":${text}"
              }

let debInfo_ =
      λ(codename : Optional DebCodename) →
        merge
          { Some =
              λ(a : DebCodename) →
                { debCodename = Some (debInfo a).debCodename
                , image = Some (debInfo a).image
                }
          , None = { debCodename = None Text, image = None Text }
          }
          codename

let ServiceDescription =
      { Type =
          { repo : Optional Text
          , version : Text
          , network : Optional Text
          , branch : Optional Text
          , cache : Optional Text
          , debCodename : Optional DebCodename
          , debRelease : Optional Text
          , debVersion : Optional Text
          , logsBucket : Optional Text
          , extraArgs : List Text
          }
      , default =
        { repo = Some "https://github.com/minaprotocol/mina"
        , network = None Text
        , branch = None Text
        , cache = None Text
        , debCodename = None DebCodename
        , debRelease = None Text
        , debVersion = None Text
        , logsBucket = None Text
        , extraArgs = [] : List Text
        }
      }

let DockerfileDescription =
      { Type =
          { service : Text
          , dockerfilePaths : List Text
          , dockerContext : Optional Text
          , timeout : Optional Text
          }
      , default = { dockerContext = None Text, timeout = Some "3600s" }
      }

let optionalBuildArg
    : Text → Optional Text → List Text
    = λ(n : Text) →
      λ(o : Optional Text) →
        merge
          { Some = λ(a : Text) → [ "--build-arg", n ++ "=" ++ a ]
          , None = [] : List Text
          }
          o

let dockerfilePathsArgs =
      λ(dockerfilePaths : List Text) →
        List/concatMap Text Text (λ(f : Text) → [ "-f", f ]) dockerfilePaths

let mkArgs
    : Text → DockerfileDescription.Type → ServiceDescription.Type → List Text
    = λ(tag : Text) →
      λ(desc : DockerfileDescription.Type) →
      λ(serviceDesc : ServiceDescription.Type) →
          optionalBuildArg "image" (debInfo_ serviceDesc.debCodename).image
        # optionalBuildArg "MINA_REPO" serviceDesc.repo
        # optionalBuildArg "network" serviceDesc.network
        # optionalBuildArg "MINA_BRANCH" serviceDesc.branch
        # optionalBuildArg
            "deb_codename"
            (debInfo_ serviceDesc.debCodename).debCodename
        # optionalBuildArg "deb_release" serviceDesc.debRelease
        # optionalBuildArg "deb_version" serviceDesc.debVersion

let mkScript
    : Text → DockerfileDescription.Type → ServiceDescription.Type → Text
    = λ(tag : Text) →
      λ(desc : DockerfileDescription.Type) →
      λ(serviceDesc : ServiceDescription.Type) →
            "docker pull gcr.io/\${PROJECT_ID}/${desc.service} || true; "
        ++  merge
              { Some = λ(ctx : Text) → ""
              , None =
                  "cat " ++ Text/concatSep " " desc.dockerfilePaths ++ " | "
              }
              desc.dockerContext
        ++  Text/concatSep
              " "
              ( List/map
                  Text
                  Text
                  escapeShellArg
                  (   [ "docker"
                      , "build"
                      , "-t"
                      , tag
                      , "--cache-from"
                      , "gcr.io/\${PROJECT_ID}/${desc.service}"
                      ]
                    # mkArgs tag desc serviceDesc
                    # serviceDesc.extraArgs
                    # merge
                        { Some =
                            λ(ctx : Text) →
                              dockerfilePathsArgs desc.dockerfilePaths # [ ctx ]
                        , None = [ "-" ]
                        }
                        desc.dockerContext
                  )
              )

let cloudBuild
    : DockerfileDescription.Type →
      ServiceDescription.Type →
        Schema.Cloudbuild.Type
    = λ(desc : DockerfileDescription.Type) →
      λ(serviceDesc : ServiceDescription.Type) →
        let tag = "gcr.io/\${PROJECT_ID}/${desc.service}:${serviceDesc.version}"

        let script = mkScript tag desc serviceDesc

        in  Schema.Cloudbuild::{
            , steps =
              [ Schema.Step::{
                , name = "gcr.io/cloud-builders/docker"
                , entrypoint = Some "bash"
                , args = Some [ "-c", script ]
                , timeout = desc.timeout
                }
              ]
            , images = Some [ tag ]
            , timeout = desc.timeout
            , logsBucket = serviceDesc.logsBucket
            }

let kanikoBuild
    : DockerfileDescription.Type →
      ServiceDescription.Type →
        Schema.Cloudbuild.Type
    = λ(desc : DockerfileDescription.Type) →
      λ(serviceDesc : ServiceDescription.Type) →
        let network =
              merge
                { Some = λ(n : Text) → "-${n}", None = "" }
                serviceDesc.network

        let imageTag = "${serviceDesc.version}${network}"

        let imageName = "gcr.io/\${PROJECT_ID}/${desc.service}"

        let image = "${imageName}:${imageTag}"

        let context =
              merge
                { Some = λ(a : Text) → "/workspace/" ++ a, None = "/workspace" }
                desc.dockerContext

        let dockerfilePathsText =
              List/fold
                Text
                desc.dockerfilePaths
                Text
                (λ(a : Text) → λ(b : Text) → a ++ " " ++ b)
                ("" : Text)

        let dockerfileStep =
              Schema.Step::{
              , name = "bash"
              , args =
                  let script =
                        ''
                        cat ${dockerfilePathsText} > ${context}/Dockerfile
                        ''

                  in  Some [ "-eEuo", "pipefail", "-c", script ]
              }

        let buildStep =
              Schema.Step::{
              , name = "gcr.io/kaniko-project/executor:latest"
              , args = Some
                  (   [ "--dockerfile=Dockerfile"
                      , "--context=dir://${context}"
                      , "--destination=${image}"
                      , "--cache=true"
                      , "--cache-ttl=24h"
                      ]
                    # mkArgs image desc serviceDesc
                  )
              , timeout = desc.timeout
              }

        let pool
            : Schema.PoolObject
            = { name =
                  "projects/o1labs-192920/locations/europe-west1/workerPools/cloudbuild-test"
              }

        let options
            : Schema.Options.Type
            = { env = None (List Text)
              , secretEnv = None Text
              , volumes = None (List Schema.Volume)
              , sourceProvenanceHash = None Text
              , machineType = None Text
              , diskSizeGb = None Text
              , dynamicSubstitutions = None Bool
              , logStreamingOption = None Text
              , logging = None Text
              , pool = Some pool
              }

        in  Schema.Cloudbuild::{
            , steps = [ dockerfileStep, buildStep ]
            , timeout = desc.timeout
            , logsBucket = serviceDesc.logsBucket
            , options = Some options
            }

let dockerBuild
    : DockerfileDescription.Type → ServiceDescription.Type → Text
    = λ(desc : DockerfileDescription.Type) →
      λ(serviceDesc : ServiceDescription.Type) →
        let tag = "${desc.service}:${serviceDesc.version}"

        let script = mkScript tag desc serviceDesc

        in  script

let services =
      { mina-archive = DockerfileDescription::{
        , service = "mina-archive"
        , dockerfilePaths = [ "dockerfiles/Dockerfile-mina-archive" ]
        , dockerContext = Some "dockerfiles"
        }
      , mina-daemon = DockerfileDescription::{
        , service = "mina-daemon"
        , dockerfilePaths = [ "dockerfiles/Dockerfile-mina-daemon" ]
        , dockerContext = Some "dockerfiles"
        }
      , mina-daemon-deb = DockerfileDescription::{
        , service = "mina-daemon-deb"
        , dockerfilePaths =
          [ "dockerfiles/stages/1-build-deps"
          , "dockerfiles/stages/2-opam-deps"
          , "dockerfiles/stages/3-toolchain"
          , "dockerfiles/stages/4-deb-builder"
          , "dockerfiles/stages/4-mina-daemon"
          ]
        , dockerContext = Some "dockerfiles"
        }
      , mina-toolchain = DockerfileDescription::{
        , service = "mina-toolchain"
        , dockerfilePaths =
          [ "dockerfiles/stages/1-build-deps"
          , "dockerfiles/stages/2-opam-deps"
          , "dockerfiles/stages/3-toolchain"
          ]
        , dockerContext = Some "dockerfiles"
        }
      , mina-deb-builder = DockerfileDescription::{
        , service = "mina-deb-builder"
        , dockerfilePaths =
          [ "dockerfiles/stages/1-build-deps"
          , "dockerfiles/stages/2-opam-deps"
          , "dockerfiles/stages/3-toolchain"
          , "dockerfiles/stages/4-deb-builder"
          ]
        , dockerContext = Some "dockerfiles"
        }
      , mina-rosetta = DockerfileDescription::{
        , service = "mina-rosetta"
        , dockerfilePaths =
          [ "dockerfiles/stages/1-build-deps"
          , "dockerfiles/stages/2-opam-deps"
          , "dockerfiles/stages/3-builder"
          , "dockerfiles/stages/4-production"
          ]
        , dockerContext = Some "dockerfiles"
        }
      }

in    { cloudBuild, kanikoBuild, dockerBuild, ServiceDescription, DebCodename }
    ⫽ services

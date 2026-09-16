let Schema =
      https://raw.githubusercontent.com/shinzui/mori-schema/3522f4a51181d73c9c90fc27a7c0838bd29ae95f/package.dhall
        sha256:dcb19e2312e790bad14e622cc98a1281cd2298c5b564a2f0d0534d3c718d8803

let scoped =
      \(name : Text) ->
      \(s : Schema.DependencyScope) ->
        Schema.Dependency.WithAugmentation
          { name
          , extraDocs = [] : List Schema.DocRef.Type
          , localPathOverride = None Text
          , kind = None Schema.DependencyKind
          , source = None Schema.DependencySource
          , scope = Some s
          , versionConstraint = None Text
          }

in  Schema.Project::{ project =
      Schema.ProjectIdentity::{ name = "ephemeral-pg"
      , namespace = "shinzui"
      , type = Schema.PackageType.Library
      , description = Some
          "Temporary PostgreSQL databases for testing with initdb caching, copy-on-write support, and native hasql integration"
      , language = Schema.Language.Haskell
      , lifecycle = Schema.Lifecycle.Active
      , domains = [ "database", "testing" ]
      , owners = [ "shinzui" ]
      }
    , repos =
      [ Schema.Repo::{ name = "ephemeral-pg"
        , github = Some "shinzui/ephemeral-pg"
        , localPath = Some "."
        }
      ]
    , packages =
      [ Schema.Package::{ name = "ephemeral-pg"
        , type = Schema.PackageType.Library
        , language = Schema.Language.Haskell
        , path = Some "."
        , description = Some
            "Temporary PostgreSQL databases for testing with initdb caching, copy-on-write support, and native hasql integration"
        , runtime =
          { deployable = False
          , exposesApi = False
          }
        , dependencies =
          [ Schema.Dependency.ByName "hasql/hasql:hasql"
          ]
        }
      , Schema.Package::{ name = "ephemeral-pg-opentelemetry"
        , type = Schema.PackageType.Library
        , language = Schema.Language.Haskell
        , path = Some "ephemeral-pg-opentelemetry"
        , description = Some "OpenTelemetry tracing for ephemeral-pg"
        , runtime =
          { deployable = False
          , exposesApi = False
          }
        , dependencies =
          [ Schema.Dependency.ByName
              "iand675/hs-opentelemetry:hs-opentelemetry-api"
          , scoped
              "iand675/hs-opentelemetry:hs-opentelemetry-exporter-handle"
              Schema.DependencyScope.Test
          , scoped
              "iand675/hs-opentelemetry:hs-opentelemetry-exporter-in-memory"
              Schema.DependencyScope.Test
          , scoped
              "iand675/hs-opentelemetry:hs-opentelemetry-instrumentation-hspec"
              Schema.DependencyScope.Test
          , scoped
              "iand675/hs-opentelemetry:hs-opentelemetry-sdk"
              Schema.DependencyScope.Test
          ]
        }
      ]
    , dependencies =
      [ "hasql/hasql:hasql"
      , "iand675/hs-opentelemetry:hs-opentelemetry-api"
      , "iand675/hs-opentelemetry:hs-opentelemetry-exporter-handle"
      , "iand675/hs-opentelemetry:hs-opentelemetry-exporter-in-memory"
      , "iand675/hs-opentelemetry:hs-opentelemetry-instrumentation-hspec"
      , "iand675/hs-opentelemetry:hs-opentelemetry-sdk"
      ]
    , dependencyRefs =
      [ Schema.MoriRef::{ namespace = "hasql"
        , name = "hasql"
        , kind = Some Schema.MoriArtifactKind.Package
        , key = Some "hasql"
        }
      , Schema.MoriRef::{ namespace = "iand675"
        , name = "hs-opentelemetry"
        , kind = Some Schema.MoriArtifactKind.Package
        , key = Some "hs-opentelemetry-api"
        }
      , Schema.MoriRef::{ namespace = "iand675"
        , name = "hs-opentelemetry"
        , kind = Some Schema.MoriArtifactKind.Package
        , key = Some "hs-opentelemetry-exporter-handle"
        }
      , Schema.MoriRef::{ namespace = "iand675"
        , name = "hs-opentelemetry"
        , kind = Some Schema.MoriArtifactKind.Package
        , key = Some "hs-opentelemetry-exporter-in-memory"
        }
      , Schema.MoriRef::{ namespace = "iand675"
        , name = "hs-opentelemetry"
        , kind = Some Schema.MoriArtifactKind.Package
        , key = Some "hs-opentelemetry-instrumentation-hspec"
        }
      , Schema.MoriRef::{ namespace = "iand675"
        , name = "hs-opentelemetry"
        , kind = Some Schema.MoriArtifactKind.Package
        , key = Some "hs-opentelemetry-sdk"
        }
      ]
    , docs =
      [ Schema.DocRef::{ key = "readme"
        , kind = Schema.DocKind.Guide
        , audience = Schema.DocAudience.User
        , description = Some "Project overview, installation, and usage guide"
        , location = Schema.DocLocation.LocalFile "README.md"
        }
      , Schema.DocRef::{ key = "changelog"
        , kind = Schema.DocKind.Notes
        , audience = Schema.DocAudience.User
        , description = Some "Release notes and version history"
        , location = Schema.DocLocation.LocalFile "CHANGELOG.md"
        }
      , Schema.DocRef::{ key = "migration-guide"
        , kind = Schema.DocKind.Guide
        , audience = Schema.DocAudience.User
        , description = Some "Migration guide from tmp-postgres"
        , location = Schema.DocLocation.LocalFile
            "docs/guides/migrating-from-tmp-postgres.md"
        }
      , Schema.DocRef::{ key = "guides"
        , kind = Schema.DocKind.Guide
        , audience = Schema.DocAudience.User
        , description = Some
            "Reader-facing guides governed by the shared user-documentation profile"
        , location = Schema.DocLocation.LocalDir "docs/guides"
        }
      ]
    , okfBundles =
      [ Schema.OkfBundle::{ name = "guides"
        , path = "docs/guides"
        , profile = Some "mori/user-documentation-profile.dhall"
        , profileBinding = Some
            ( Schema.ProfileBinding.Published
                Schema.PinnedImport::{ publisher = "shinzui/okf-profiles"
                , publisherRef = Some Schema.MoriRef::{ namespace = "shinzui"
                  , name = "okf-profiles"
                  }
                , export = Some "documentation.userDocumentation"
                , version = Some "v0.15.0"
                , pin = Some
                    "sha256:e1e7eaac9d08fd3409fe0d19057dba5634a4186733ccbf28323e9aa2a2512dc0"
                }
            )
        , okfVersion = "0.2"
        , description = Some
            "Task-oriented migration and fixture guides plus the temporary-root explanation"
        }
      ]
    }

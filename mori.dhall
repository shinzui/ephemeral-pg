let Schema =
      https://raw.githubusercontent.com/shinzui/mori-schema/9b1d6eea8027ae57576cf0712c0b9167fccbc1a9/package.dhall
        sha256:a19f5dd9181db28ba7a6a1b77b5ab8715e81aba3e2a8f296f40973003a0b4412

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
          [ Schema.Dependency.ByName "hasql/hasql"
          ]
        }
      ]
    , dependencies =
      [ "hasql/hasql"
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
        , location = Schema.DocLocation.LocalDir "docs"
        }
      ]
    }

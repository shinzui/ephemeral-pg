let Schema =
      https://raw.githubusercontent.com/shinzui/mori-schema/28dfc529336f0c92a846f074b5f19c6442394a84/package.dhall
        sha256:dda2cb9c528a0edac9c5ba7aeb14517d153ec612ec70c30115bb6749f15df15b

in  { project =
      { name = "ephemeral-pg"
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
      [ { name = "ephemeral-pg"
        , github = Some "shinzui/ephemeral-pg"
        , gitlab = None Text
        , git = None Text
        , localPath = Some "."
        }
      ]
    , packages =
      [ { name = "ephemeral-pg"
        , type = Schema.PackageType.Library
        , language = Schema.Language.Haskell
        , path = Some "."
        , description = Some
            "Temporary PostgreSQL databases for testing with initdb caching, copy-on-write support, and native hasql integration"
        , visibility = Schema.Visibility.Public
        , runtime =
          { deployable = False
          , exposesApi = False
          }
        , dependencies =
          [ Schema.Dependency.ByName "hasql/hasql"
          ]
        , docs = [] : List Schema.DocRef
        , config = [] : List Schema.ConfigItem
        }
      ]
    , bundles = [] : List Schema.PackageBundle
    , dependencies =
      [ "hasql/hasql"
      ]
    , apis = [] : List Schema.Api
    , agents = [] : List Schema.AgentHint
    , standards = [] : List Text
    , docs =
      [ { key = "readme"
        , kind = Schema.DocKind.Guide
        , audience = Schema.DocAudience.User
        , description = Some "Project overview, installation, and usage guide"
        , location = Schema.DocLocation.LocalFile "README.md"
        }
      , { key = "changelog"
        , kind = Schema.DocKind.Notes
        , audience = Schema.DocAudience.User
        , description = Some "Release notes and version history"
        , location = Schema.DocLocation.LocalFile "CHANGELOG.md"
        }
      , { key = "migration-guide"
        , kind = Schema.DocKind.Guide
        , audience = Schema.DocAudience.User
        , description = Some "Migration guide from tmp-postgres"
        , location = Schema.DocLocation.LocalDir "docs"
        }
      ]
    }

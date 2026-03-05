let Schema =
      https://raw.githubusercontent.com/shinzui/mori-schema/4412469f2960b8faa48c123451bf90c0d3400db3/package.dhall
        sha256:2e416c2d8c28c0b3b217cab47cc6d9e8bb9bec34b87d476edbb0d6d0863d1401

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
      , origin = Schema.Origin.Own
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

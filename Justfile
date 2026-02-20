default:
  just --list

# Build the library
build:
  cabal build

# Run tests
test:
  cabal test

# Generate haddock documentation
haddock:
  cabal haddock --haddock-hyperlink-source --haddock-quickjump

# Open generated haddock docs in browser
haddock-open: haddock
  open "$(cabal haddock --haddock-hyperlink-source --haddock-quickjump 2>&1 | tail -1)"

# Create a source distribution tarball
sdist:
  cabal sdist

# Check the sdist for common packaging issues
sdist-check:
  cabal check

# Candidate upload to Hackage (does not publish)
upload-candidate: sdist
  cabal upload "$(cabal sdist 2>&1 | tail -1)"

# Publish to Hackage (permanent, cannot be undone)
publish: sdist
  @echo "This will permanently publish to Hackage. Press Ctrl+C to cancel."
  @read -r _
  cabal upload --publish "$(cabal sdist 2>&1 | tail -1)"

# Upload candidate haddock docs to Hackage
upload-docs-candidate:
  cabal haddock --haddock-for-hackage --haddock-hyperlink-source --haddock-quickjump
  cabal upload --documentation "$(cabal haddock --haddock-for-hackage 2>&1 | tail -1)"

# Publish haddock docs to Hackage (permanent)
upload-docs:
  cabal haddock --haddock-for-hackage --haddock-hyperlink-source --haddock-quickjump
  cabal upload --publish --documentation "$(cabal haddock --haddock-for-hackage 2>&1 | tail -1)"

# Full release workflow: check, test, publish package + docs
release: sdist-check test publish upload-docs

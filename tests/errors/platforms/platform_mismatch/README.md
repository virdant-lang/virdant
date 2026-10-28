# platforms/platform_mismatch

Project-style error test: a module whose ports do not exactly match its
platform's ports fails `vir check`. The source imports `ice40` and declares
`pub mod Top for ice40::IceSugar` with a port set that deliberately
mismatches the platform definition.

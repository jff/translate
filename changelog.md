# 2010.01.24  Joao F. Ferreira  <joao@joaoff.com>

 - Added support for language detection: `Text.Language.Detect`
 - `Text.Translate` is now `Text.Language.Translate`, but I've maintained
   `Text.Translate` for backward compatibility
 - Fixed bug with URI escaping. Google's API is expecting an ASCII string
   that is first escaped with UTF-8.
 - Refactored parts of the module to avoid repeated code

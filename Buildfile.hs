module Main where

import Blag.Agda qualified as Agda
import Blag.PostInfo
import Blag.Prelude
import Blag.Routing
import Blag.Style
import Blag.Template
import Blag.Template.Pandoc qualified as Pandoc
import Blag.Template.Pandoc.Builder qualified as Builder
import Blag.Template.Pandoc.Citeproc qualified as Citeproc
import Blag.Template.TagSoup qualified as TagSoup
import Control.Concurrent (threadDelay)
import Control.Monad (forM, forM_, forever, join)
import Control.Monad.IO.Class (MonadIO)
import Data.Default.Class
import Data.Foldable (Foldable (fold))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (isPrefixOf)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe, maybeToList)
import Data.Monoid (Endo (Endo, appEndo))
import Data.MultiMap qualified as MultiMap
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Base (when)
import System.IO.Temp (getCanonicalTemporaryDirectory, withTempDirectory)
import System.Process (ProcessHandle, cleanupProcess)

outDir, tmpDir :: FilePath
outDir = "_site"
tmpDir = "_cache"

main :: IO ()
main = shakeArgs shakeOptions {shakeFiles = tmpDir, shakeProgress = progressSimple} $ do
  --------------------------------------------------------------------------------
  -- Agda libraries

  standardLibrary <- Agda.getStandardLibrary "agda-stdlib"
  let localLibraries = [postLibrary]
  let otherLibraries = []
  let ?agdaLibraries = [standardLibrary] <> localLibraries <> otherLibraries

  --------------------------------------------------------------------------------
  -- Routing table

  routingTable <-
    fmap mconcat . sequence $
      [ ["index.html"] |-> outDir </> "index.html",
        [postSrcDir </> "*.md"] |-> postRouter,
        ["pages/recipes.html"] |-> outDir </> "recipes" </> "index.html",
        [recipeSrcDir </> "*.md"] |-> recipeRouter,
        ["404.html"] |-> outDir </> "404.html",
        ["pages/pubs.html"] |-> outDir </> "pubs" </> "index.html",
        [assetSrcDir <//> "*"] |-> assetRouter,
        [styleSrcDir </> "*.scss"] |-> styleRouter,
        ["rss.xml"] |-> outDir </> "rss.xml"
      ]
  let ?routingTable = routingTable

  --------------------------------------------------------------------------------
  -- Cached file, template, and metadata getters

  cachedGetSiteMetadata <- newCache getSiteMetadata
  let ?getSiteMetadata = cachedGetSiteMetadata
  cachedGetFileWithMetadata <- newCache getFileWithMetadata
  let ?getFileWithMetadata = cachedGetFileWithMetadata
  cachedGetPostMetadata <- newCache getPostMetadata
  let ?getPostMetadata = cachedGetPostMetadata
  cachedGetTemplateFile <- newCache getTemplateFile
  let ?getTemplateFile = getTemplateFile
  cachedGetRecipeMetadata <- newCache getRecipeMetadata
  let ?getRecipeMetadata = cachedGetRecipeMetadata

  --------------------------------------------------------------------------------
  -- Agda link fixers

  agdaLinkFixer <- getAgdaLinkFixer (Just standardLibrary) localLibraries otherLibraries
  let ?agdaLinkFixer = agdaLinkFixer

  --------------------------------------------------------------------------------
  -- Phony targets

  "build" ~> do
    need (outputs ?routingTable)

  "clean" ~> do
    removeFilesAfter tmpDir ["//*"]

  "clobber" ~> do
    removeFilesAfter outDir ["//*"]
    removeFilesAfter tmpDir ["//*"]

  --------------------------------------------------------------------------------
  -- File targets

  alternatives $ do
    styleRules -- Style sheets
    assetRules -- Static assets

  -- Publications page
  pubsRules

  -- Posts
  outDir </> "index.html" %> \out -> do
    src <- routeSrc out
    postMetadata <- ?getPostMetadata ()
    (fileMetadata, indexHtmlTemplate) <- ?getFileWithMetadata src
    applyAsTemplate (postMetadata <> fileMetadata) indexHtmlTemplate
      >>= applyTemplate "default.html" fileMetadata
      <&> postprocessHtml5 outDir out
      >>= writeFile' out

  outDir </> "rss.xml" %> \out -> do
    src <- routeSrc out
    postMetadata <- ?getPostMetadata ()
    (fileMetadata, rssXmlTemplate) <- ?getFileWithMetadata src
    readFile' src
      >>= applyAsTemplate (fileMetadata <> postMetadata)
      >>= writeFile' out

  postRules

  -- Recipes
  outDir </> "recipes" </> "index.html" %> \out -> do
    src <- routeSrc out
    recipeMetadata <- ?getRecipeMetadata ()
    (fileMetadata, recipesHtmlTemplate) <- ?getFileWithMetadata src
    applyAsTemplate (recipeMetadata <> fileMetadata) recipesHtmlTemplate
      >>= applyTemplate "default.html" fileMetadata
      <&> postprocessHtml5 outDir out
      >>= writeFile' out

  recipeRules

  -- 404.html
  outDir </> "404.html" %> \out -> do
    metadata <- ?getSiteMetadata ()
    readFile' "404.html"
      >>= applyTemplate "default.html" metadata
      <&> postprocessHtml5 outDir out
      >>= writeFile' out

--------------------------------------------------------------------------------
-- Posts

postSrcDir, postTmp1Dir, postTmp2Dir, postOutDir :: FilePath
postSrcDir = "posts"
postTmp1Dir = tmpDir </> "stage1" </> "posts" -- Render .lagda.md to .md
postTmp2Dir = tmpDir </> "stage2" </> "posts" -- Render .md to .html
postOutDir = outDir -- NOTE: cannot rely on 'postOutDir' to test if a FilePath is an output

postRouter :: (?agdaLibraries :: [Agda.Library]) => FilePath -> Rules [(Maybe Anchor, FilePath)]
postRouter src = do
  let postSrc = makeRelative postSrcDir src
  PostInfo {..} <- parsePostInfo postSrc
  let htmlBody = postTmp2Dir </> postSrc
  let out = postOutDir </> year </> month </> day </> fileName </> "index.html"
  if Agda.isAgdaFile src
    then do
      highlightAgda <- Agda.markdownOutputPath postTmp1Dir ?agdaLibraries src
      return [(Nothing, highlightAgda), (Just "html-body", htmlBody), (Nothing, out)]
    else do return [(Just "html-body", htmlBody), (Nothing, out)]

isPostSrc, isPostTmp1, isPostTmp2 :: FilePath -> Bool
isPostSrc src = postSrcDir `isPrefixOf` src
isPostTmp1 tmp = postTmp1Dir `isPrefixOf` tmp
isPostTmp2 tmp = postTmp2Dir `isPrefixOf` tmp

isPostOut :: (?routingTable :: RoutingTable) => FilePath -> Bool
isPostOut out = isNothing (routeNext out) && maybe False isPostSrc (routeSrc out)

postRules ::
  ( ?routingTable :: RoutingTable,
    ?getTemplateFile :: FilePath -> Action Template,
    ?getFileWithMetadata :: FilePath -> Action (Metadata, Text),
    ?agdaLibraries :: [Agda.Library],
    ?agdaLinkFixer :: Url -> Url
  ) =>
  Rules ()
postRules = do
  -- Compile literate Agda to Markdown & HTML
  isPostTmp1 ?> \next -> do
    prev <- routePrev next
    agdaToHtml prev

  -- Compile Markdown to HTML
  isPostTmp2 ?> \next -> do
    (out, prev, src) <- (,,) <$> route next <*> routePrev next <*> routeSrc next
    let maybeAgdaLinkFixer = if Agda.isAgdaFile src then Just (Pandoc.withUrls ?agdaLinkFixer) else Nothing
    readFile' prev
      >>= markdownToPandoc
      >>= processCitations
      <&> walk (shiftHeadersBy 2)
      <&> fromMaybe id maybeAgdaLinkFixer
      >>= pandocToHtml5 -- postprocessHtml5 in next rule
      >>= writeFile' next

  -- Apply templates
  isPostOut ?> \out -> do
    (prev, src) <- (,) <$> routePrev out <*> routeSrc out
    metadata <- fst <$> ?getFileWithMetadata src
    readFile' prev
      >>= applyTemplates ["post.html", "default.html"] metadata
      <&> postprocessHtml5 outDir out
      >>= writeFile' out

--------------------------------------------------------------------------------
-- Recipes

recipeSrcDir, recipeOutDir :: FilePath
recipeSrcDir = "recipes"
recipeOutDir = outDir </> "recipes"

isRecipeSrc :: FilePath -> Bool
isRecipeSrc src = recipeSrcDir `isPrefixOf` src

recipeRouter :: (?agdaLibraries :: [Agda.Library]) => FilePath -> Rules FilePath
recipeRouter src =
  return $ recipeOutDir </> dropExtension (makeRelative recipeSrcDir src) </> "index.html"

recipeRules ::
  ( ?routingTable :: RoutingTable,
    ?getTemplateFile :: FilePath -> Action Template,
    ?getFileWithMetadata :: FilePath -> Action (Metadata, Text)
  ) =>
  Rules ()
recipeRules = do
  recipeOutDir <//> "*" %> \out -> do
    src <- routeSrc out
    metadata <- fst <$> ?getFileWithMetadata src
    readFile' src
      >>= markdownToPandoc
      >>= processCitations
      <&> walk (shiftHeadersBy 2)
      >>= pandocToHtml5
      >>= applyTemplates ["recipe.html", "default.html"] metadata
      <&> postprocessHtml5 outDir out
      >>= writeFile' out

--------------------------------------------------------------------------------
-- Agda

postLibrary :: Agda.Library
postLibrary =
  Agda.Library
    { libraryRoot = "",
      includePaths = [postSrcDir],
      canonicalBaseUrl = "https://wen.works/"
    }

agdaToHtml :: (?agdaLibraries :: [Agda.Library]) => FilePath -> Action ()
agdaToHtml src = do
  need [src]
  command_ [] "agda" $
    concat
      [ ["--verbose=0"],
        Agda.markdownArgs postTmp1Dir,
        Agda.libraryArgs ?agdaLibraries,
        [src]
      ]

getAgdaLinkFixer ::
  ( MonadIO m,
    MonadFail m,
    ?routingTable :: RoutingTable
  ) =>
  Maybe Agda.Library ->
  [Agda.Library] ->
  [Agda.Library] ->
  m (Url -> Url)
getAgdaLinkFixer standardLibrary localLibraries otherLibraries = do
  let maybeBuiltinLinkFixer = Agda.makeBuiltinLinkFixer <$> standardLibrary
  maybeStandardLibraryLinkFixer <- traverse Agda.makeLibraryLinkFixer standardLibrary
  localLinkFixers <- traverse Agda.makeLocalLinkFixer localLibraries
  otherLinkFixers <- traverse Agda.makeLibraryLinkFixer otherLibraries
  let linkFixers =
        [ maybeToList maybeBuiltinLinkFixer,
          maybeToList maybeStandardLibraryLinkFixer,
          otherLinkFixers,
          localLinkFixers
        ]
  return . appEndo . mconcat . fmap Endo . concat $ linkFixers

--------------------------------------------------------------------------------
-- Style Sheets

styleSrcDir, styleOutDir :: FilePath
styleSrcDir = "sass"
styleOutDir = outDir </> "assets" </> "css"

styleRouter :: FilePath -> Rules FilePath
styleRouter src =
  return $
    styleOutDir </> makeRelative styleSrcDir src -<.> "css"

styleRules :: (?routingTable :: RoutingTable) => Rules ()
styleRules =
  let sassOptions =
        def
          { sassIncludePaths = Just [styleSrcDir],
            sassImporters = Just [minCssImporter styleSrcDir 1]
          }
   in styleOutDir </> "*.css" %> \out -> do
        src <- routeSrc out
        compileSassWith sassOptions src
          >>= minifyCSS
          >>= writeFile' out

--------------------------------------------------------------------------------
-- Assets

assetSrcDir, assetOutDir :: FilePath
assetSrcDir = "assets"
assetOutDir = outDir </> "assets"

assetRouter :: FilePath -> Rules FilePath
assetRouter src =
  return $
    assetOutDir </> makeRelative assetSrcDir src

assetRules :: (?routingTable :: RoutingTable) => Rules ()
assetRules =
  assetOutDir <//> "*" %> \out -> do
    src <- routeSrc out
    copyFile' src out

--------------------------------------------------------------------------------
-- Markdown to HTML compilation

shiftHeadersBy :: Int -> Block -> Block
shiftHeadersBy n (Header l attr body) = Header (l + n) attr body
shiftHeadersBy n x = x

highlightStyle :: Pandoc.HighlightStyle
highlightStyle = Pandoc.pygments

markdownToPandoc :: Text -> Action Pandoc
markdownToPandoc =
  Pandoc.runPandoc . Pandoc.readMarkdown readerOpts

pandocToHtml5 :: Pandoc -> Action Text
pandocToHtml5 =
  Pandoc.runPandoc . Pandoc.writeHtml5String writerOpts

processCitations :: Pandoc -> Action Pandoc
processCitations =
  Pandoc.runPandoc . Citeproc.processCitations

postprocessHtml5 :: FilePath -> FilePath -> Text -> Text
postprocessHtml5 outDir out html5 =
  html5 & TagSoup.withUrls (implicitIndexFile . relativizeUrl outDir out)
    & Pandoc.postprocessHtml5

markdownDialect :: Extensions
markdownDialect = Pandoc.pandocExtensions

readerOpts :: ReaderOptions
readerOpts =
  def
    { readerExtensions = markdownDialect
    }

writerOpts :: WriterOptions
writerOpts =
  def
    { writerHTMLMathMethod = KaTeX "https://wen.works/",
      writerEmailObfuscation = JavascriptObfuscation,
      writerHighlightStyle = Just highlightStyle
    }

--------------------------------------------------------------------------------
-- Metadata

-- | Get the default metadata for the website.
getSiteMetadata :: () -> Action Metadata
getSiteMetadata () = do
  siteMetadata <- readYaml' "site.yml"
  buildDateFld <- currentDateField rfc822DateFormat "build_date"
  let metadata = mconcat [siteMetadata, buildDateFld]
  return $ constField "site" metadata

-- | Get a file body and its metadata, including derived metadata.
--
--   This function adds the following metadata fields,
--   in addition to the metadata added by 'getSiteMetadata':
--
--     - @url@: The URL to the output file.
--     - @body@: The body of the source file.
--     - @source@: The path to the source file.
--     - @modified_date@: The date at which the file was last modified, in the ISO8601 format.
--     - @build_date@: The date at which the is website was last built, in the RFC822 format.
--     - @highlight-css@: The CSS for syntax highlighting, if the original file specifies 'highlight'.
getFileWithMetadata ::
  ( ?routingTable :: RoutingTable,
    ?getSiteMetadata :: () -> Action Metadata
  ) =>
  FilePath ->
  Action (Metadata, Text)
getFileWithMetadata src = do
  out <- route src
  let url = "/" <> makeRelative outDir out
  siteMetadata <- ?getSiteMetadata ()
  (fileMetadata, body) <- readFileWithMetadata' src
  let urlFld = constField "url" url
  let bodyFld = constField "body" body
  let sourceFld = constField "source" src
  modifiedDateFld <- lastModifiedISO8601Field src "modified_date"
  let maybeHighlightCssFld = case fileMetadata ^. "highlight" of
        Just True -> constField "highlight-css" (Pandoc.styleToCss highlightStyle)
        _ -> mempty
  let metadata = mconcat [siteMetadata, fileMetadata, urlFld, bodyFld, sourceFld, modifiedDateFld, maybeHighlightCssFld]
  return (metadata, body)

-- | Get a metadata object representing all posts.
--
--   This function adds the following metadata fields for each post,
--   in addition to the metadata added by 'getFileWithMetadata':
--
--   - @body_html@: The rendered HTML body of the source file.
--   - @date@: The date of the post, in a human readable format.
--   - @date_rfc822@: The date of the post, in the RFC822 date format.
--   - @teaser_html@: The rendered HTML teaser for the post.
--   - @teaser_plain@: The plain text teaser for the post.
getPostMetadata ::
  ( ?routingTable :: RoutingTable,
    ?getFileWithMetadata :: FilePath -> Action (Metadata, Text)
  ) =>
  () ->
  Action Metadata
getPostMetadata () = do
  -- Get posts from routing table
  let postSrcs = filter isPostSrc (sources ?routingTable)
  -- Gather metadata for each post
  postsMetadata <- forM postSrcs $ \src -> do
    -- Get output file for URL and html-body anchor for teaser
    (out, ankHtmlBody) <- (,) <$> route src <*> routeAnchor "html-body" src
    let url = "/" <> makeRelative outDir out
    fileMetadata <- fst <$> ?getFileWithMetadata src
    bodyHtml <- readFile' ankHtmlBody
    let bodyHtmlFld = constField "body_html" bodyHtml
    dateFld <- postDateField "%a %-d %b, %Y" src "date"
    dateRfc822Fld <- postDateField rfc822DateFormat src "date_rfc822"
    teaserHtmlFld <- htmlTeaserField url bodyHtml "teaser_html"
    teaserPlainFld <- textTeaserField bodyHtml "teaser_plain"
    return $ mconcat [fileMetadata, bodyHtmlFld, dateFld, dateRfc822Fld, teaserHtmlFld, teaserPlainFld]
  return $ constField "post" (reverse postsMetadata)

-- | Get a metadata object representing all recipes.
getRecipeMetadata ::
  ( ?routingTable :: RoutingTable,
    ?getFileWithMetadata :: FilePath -> Action (Metadata, Text)
  ) =>
  () ->
  Action Metadata
getRecipeMetadata () = do
  -- Get recipes from routing table
  let recipeSrcs = filter isRecipeSrc (sources ?routingTable)
  -- Gather metadata for each recipe
  recipesMetadata <- forM recipeSrcs $ \src -> do
    -- Get output file for URL and html-body anchor for teaser
    out <- route src
    let url = "/" <> makeRelative outDir out
    fst <$> ?getFileWithMetadata src
  return $ constField "recipe" recipesMetadata

-- | Get a template from the @templates/@ directory.
getTemplateFile :: FilePath -> Action Template
getTemplateFile inputFile = do
  let inputPath = "templates" </> inputFile
  need [inputPath]
  compileTemplateFile inputPath

--------------------------------------------------------------------------------
-- Pubs

pubsRules ::
  ( ?routingTable :: RoutingTable,
    ?getTemplateFile :: FilePath -> Action Template,
    ?getFileWithMetadata :: FilePath -> Action (Metadata, Text)
  ) =>
  Rules ()
pubsRules = do
  outDir </> "pubs" </> "index.html" %> \out -> do
    src <- routeSrc out
    (fileMetadata, pubsHtmlTemplate) <- ?getFileWithMetadata src

    -- Get language and locale from 'site.lang'
    (lang, locale) <- getLangAndLocale fileMetadata

    -- Get citation style from 'citation-style'
    csl <- getStyle fileMetadata

    -- Get references from 'bibliography' and remove me from authors based on
    -- 'site.author.name', then construct a map from reference ids to types
    myName <- fileMetadata ^. "site.author.name"
    refs <- map (removeAuthor myName) <$> getReferences locale fileMetadata
    let refTypeByRefId = Map.fromList [(referenceId, referenceType) | Reference {..} <- refs]

    -- Gather the citations used in the fields of the bibliography
    let cits = Citeproc.getCitationsFromReferences refs

    -- Process the references and citations using citeproc
    let citeprocOptions = Citeproc.defaultCiteprocOptions {linkBibliography = True}
    let Result {..} = Citeproc.citeproc citeprocOptions csl (Just lang) refs cits
    forM_ resultWarnings (putWarn . Text.unpack)

    -- NOTE: assumes that citeproc does not change the order of these citations
    let citsByItemIds = Map.fromList $ zip (map Citeproc.getItemIds cits) resultCitations
    let refsByRefType = MultiMap.fromList $ do
          ref@(refId, refIls) <- resultBibliography
          -- TODO: throw an error when lookup fails
          refType <- maybeToList (Map.lookup (ItemId refId) refTypeByRefId)
          return (refType, ref)

    -- Render each section
    sections <- forM pubSections $ \sec@Section {..} -> do
      let refsForSection = concatMap (`MultiMap.lookup` refsByRefType) sectionReferenceTypes
      let blocksForSection =
            mconcat
              [ Builder.headerWith (sectionAnchor sec, [], []) 3 (Builder.str sectionTitle),
                Builder.bulletList [Builder.divWith (key, [], []) (Builder.plain item) | (key, item) <- refsForSection]
              ]
      let withResolvedCitations = fmap (Citeproc.insertCitations citsByItemIds) blocksForSection
      return $ if null refsForSection then mempty else withResolvedCitations

    let body = Builder.doc (fold sections)
    body <- Pandoc.runPandoc (Pandoc.writeHtml5String writerOpts body)
    let bodyFld = constField "body" body

    applyAsTemplate (bodyFld <> fileMetadata) pubsHtmlTemplate
      >>= applyTemplates ["page.html", "default.html"] fileMetadata
      <&> postprocessHtml5 outDir out
      >>= writeFile' out

data Section = Section
  { sectionReferenceTypes :: [Text],
    sectionTitle :: Text
  }

sectionAnchor :: Section -> Text
sectionAnchor Section {..} = Text.toLower (Text.replace " " "-" sectionTitle)

pubSections :: [Section]
pubSections =
  [ Section ["manuscript"] "Drafts",
    Section ["article-journal"] "Journal Articles",
    Section ["book"] "Books",
    Section ["chapter", "paper-conference"] "Conference and Workshop Papers",
    Section ["thesis"] "Theses",
    Section ["speech"] "Talks",
    Section ["notype"] "Public Houses"
  ]

-- Get citation style from 'citation-style'.
getStyle :: Metadata -> Action (Style Inlines)
getStyle metadata =
  metadata ^. "citation-style"
    >>= readFile'
    >>= Citeproc.parseStyle (\_ -> return mempty)
    >>= liftEither (Text.unpack . Citeproc.prettyCiteprocError)

-- Get references from 'bibliography', remove my name:
getReferences :: Locale -> Metadata -> Action [Reference Inlines]
getReferences locale metadata = do
  bib <- readFile' =<< metadata ^. "bibliography"
  Pandoc.runPandoc $
    Citeproc.getReferences (Just locale) =<< Pandoc.readBibTeX def bib

-- Get lang and corresponding locale from 'site.lang'.
getLangAndLocale :: Metadata -> Action (Lang, Locale)
getLangAndLocale metadata = do
  lang <- metadata ^. "site.lang"
  lang <- liftEither id (Citeproc.parseLang lang)
  locale <- liftEither (Text.unpack . Citeproc.prettyCiteprocError) (Citeproc.getLocale lang)
  return (lang, locale)

-- | Remove an author from the reference author field.
removeAuthor :: Name -> Reference Inlines -> Reference Inlines
removeAuthor name ref@Reference {..} =
  ref {referenceVariables = Map.adjust (removeAuthorFromVal name) "author" referenceVariables}
  where
    removeAuthorFromVal name (NamesVal names) = NamesVal (filter (/= name) names)
    removeAuthorFromVal _name val = val
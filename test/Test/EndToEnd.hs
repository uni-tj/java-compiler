module Test.EndToEnd(genEndToEndTests) where
import           Compiler               (DebugMode (NoDebug), compile)
import           Control.Lens           (set, view, (^.))
import           Control.Monad          (join)
import           Data.Functor           ((<&>))
import           Data.List.Extra        (nub)
import           Debug.Trace            (traceShowId)
import           System.Directory.Extra (listDirectory)
import           System.FilePath.Lens   (basename, directory, extension)
import           System.Process.Extra   (CmdSpec (ShellCommand),
                                         CreateProcess (CreateProcess, child_group, child_user, close_fds, cmdspec, create_group, create_new_console, cwd, delegate_ctlc, detach_console, env, new_session, std_err, std_in, std_out, use_process_jobs),
                                         StdStream (Inherit), readCreateProcess)
import           Test.HUnit             (Test (TestCase, TestLabel, TestList),
                                         assertEqual)

testDir = "./test/Test/EndToEnd/"

compileAndRun :: FilePath -> IO String
compileAndRun filePath = do
  compile filePath NoDebug
  readCreateProcess javaProcess $ filePath ^. basename
  where javaProcess = CreateProcess
          { cmdspec = ShellCommand $ "java " ++ filePath ^. basename
          , cwd = Just testDir
          , env = Nothing
          , std_in = Inherit
          , std_out = Inherit
          , std_err = Inherit
          , close_fds = True
          , create_group = False
          , delegate_ctlc = False
          , detach_console = False
          , create_new_console = False
          , new_session = False
          , child_group = Nothing
          , child_user = Nothing
          , use_process_jobs = False
          }

genEndToEndTests :: IO Test
genEndToEndTests = do
  dir <- traceShowId <$> listDirectory testDir
  let tests = traceShowId $ nub $ set directory testDir . set extension "" <$> filter ((".class" /=) . view extension) dir
  return $
    TestList $ tests <&> \test ->
      TestLabel (test ^. basename) $
      TestCase $ join $ assertEqual "" <$> readFile (spec test) <*> compileAndRun (java test)
  where java = set extension "java"
        spec = set extension "spec"

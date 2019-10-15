import Test.Hspec
import Syntax
import Programs
import qualified ConcreteInterpreter as Concrete
import qualified DirectStyleInterpreter as Direct
import qualified TypeChecker as Checker

main :: IO ()
main = hspec $ mapM_ testValid [addMul, bools, copy, block, branch0, branch1,
                                branchIf, loop]

testValid program = it ("Check shared and direct-style interpreters on:\n" ++
                         show program ++ ".") $ do
    let directVs = Direct.run program
    let concreteVs = Concrete.run program
    let checkerTys = (\(Right tys) -> tys) (Checker.check' program)
    directVs `shouldBe` concreteVs
    checkerTys `shouldBe` (map typeOf directVs)

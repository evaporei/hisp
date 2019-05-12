import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Expr (Expr(..))
import Env (Env, defaultEnv)
import Tokenize (tokenize)
import Parse (parse)
import Eval (eval)

interpret :: String -> Expr
interpret input = case (parse (tokenize input)) of
                    Left err -> error (show err)
                    Right (expr, _) -> case (eval defaultEnv expr) of
                                         Left err -> error (show err)
                                         Right (newEnv, expr) -> expr

interpretAndGetEnv :: String -> Env
interpretAndGetEnv input = case (parse (tokenize input)) of
                    Left err -> error (show err)
                    Right (expr, _) -> case (eval defaultEnv expr) of
                                         Left err -> error (show err)
                                         Right (newEnv, expr) -> newEnv

interpretWithEnv :: Env -> String -> Expr
interpretWithEnv env input = case (parse (tokenize input)) of
                    Left err -> error (show err)
                    Right (expr, _) -> case (eval env expr) of
                                         Left err -> error (show err)
                                         Right (newEnv, expr) -> expr

main :: IO ()
main = hspec $ do
  describe "Tokenize |> Parse |> Eval" $ do
    it "should sum numbers" $ do
      interpret "(+ 2 3 4)" `shouldBe` Number 9.0

    it "should subtract numbers" $ do
      interpret "(- 2 3 6)" `shouldBe` Number (-7.0)

    describe "should compare equality" $ do
      it "when it is equal" $ do
        interpret "(= 2 2 2)" `shouldBe` Boolean True

      it "when it is not equal" $ do
        interpret "(= 2 4 1)" `shouldBe` Boolean False 

    describe "should compare if it is greater" $ do
      it "when it is" $ do
        interpret "(> 6 4 2)" `shouldBe` Boolean True 

      it "when it is not" $ do
        interpret "(> 2 4 6)" `shouldBe` Boolean False 

      it "when there is a wrong number in the sequence" $ do
        interpret "(> 6 4 5 3)" `shouldBe` Boolean False 

    describe "should compare if it is greater than or equal" $ do
      it "when it is greater" $ do
        interpret "(>= 6 4 2)" `shouldBe` Boolean True 

      it "when it is equal" $ do
        interpret "(>= 4 4 4)" `shouldBe` Boolean True 

      it "when it is not" $ do
        interpret "(>= 2 4 6)" `shouldBe` Boolean False 

      it "when there is a wrong number in the sequence" $ do
        interpret "(>= 6 4 5 3)" `shouldBe` Boolean False 

    describe "should compare if it is smaller" $ do
      it "when it is" $ do
        interpret "(< 2 4 6)" `shouldBe` Boolean True

      it "when it is not" $ do
        interpret "(< 6 4 2)" `shouldBe` Boolean False 

      it "when there is a wrong number in the sequence" $ do
        interpret "(> 3 5 4 6)" `shouldBe` Boolean False 

    describe "should compare if it is smaller than or equal" $ do
      it "when it is smaller" $ do
        interpret "(<= 2 4 6)" `shouldBe` Boolean True 

      it "when it is equal" $ do
        interpret "(<= 4 4 4)" `shouldBe` Boolean True 

      it "when it is not" $ do
        interpret "(<= 6 4 2)" `shouldBe` Boolean False 

      it "when there is a wrong number in the sequence" $ do
        interpret "(<= 3 5 4 6)" `shouldBe` Boolean False 

    describe "should define" $ do
      it "a number" $ do
        let newEnv = interpretAndGetEnv "(def five 5.0)"
         in interpretWithEnv newEnv "(+ 5 five)" `shouldBe` Number 10.0

      it "a lambda" $ do
        let newEnv = interpretAndGetEnv "(def add-one (fn (x) (+ 1 x)))"
         in interpretWithEnv newEnv "(add-one 5)" `shouldBe` Number 6.0

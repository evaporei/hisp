import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Expr (Expr(..))
import Env (Env, defaultEnv)
import Tokenize (tokenize)
import Parse (parse)
import Eval (eval)

interpret :: String -> Expr
interpret input = case parse (tokenize input) of
                    Left err -> error (show err)
                    Right (expr, _) -> case eval defaultEnv expr of
                                         Left err -> error (show err)
                                         Right (newEnv, expr) -> expr

interpretAndGetEnv :: String -> Env
interpretAndGetEnv input = case parse (tokenize input) of
                    Left err -> error (show err)
                    Right (expr, _) -> case eval defaultEnv expr of
                                         Left err -> error (show err)
                                         Right (newEnv, expr) -> newEnv

interpretWithEnv :: Env -> String -> Expr
interpretWithEnv env input = case parse (tokenize input) of
                    Left err -> error (show err)
                    Right (expr, _) -> case eval env expr of
                                         Left err -> error (show err)
                                         Right (newEnv, expr) -> expr

main :: IO ()
main = hspec $
  describe "Tokenize |> Parse |> Eval" $ do
    it "should sum numbers" $
      interpret "(+ 2 3 4)" `shouldBe` Number 9.0

    it "should subtract numbers" $
      interpret "(- 2 3 6)" `shouldBe` Number (-7.0)

    describe "should compare equality" $ do
      it "when it is equal" $
        interpret "(= 2 2 2)" `shouldBe` Boolean True

      it "when it is not equal" $
        interpret "(= 2 4 1)" `shouldBe` Boolean False 

    describe "should compare if it is greater" $ do
      it "when it is" $
        interpret "(> 6 4 2)" `shouldBe` Boolean True 

      it "when it is not" $
        interpret "(> 2 4 6)" `shouldBe` Boolean False 

      it "when there is a wrong number in the sequence" $
        interpret "(> 6 4 5 3)" `shouldBe` Boolean False 

    describe "should compare if it is greater than or equal" $ do
      it "when it is greater" $
        interpret "(>= 6 4 2)" `shouldBe` Boolean True 

      it "when it is equal" $
        interpret "(>= 4 4 4)" `shouldBe` Boolean True 

      it "when it is not" $
        interpret "(>= 2 4 6)" `shouldBe` Boolean False 

      it "when there is a wrong number in the sequence" $
        interpret "(>= 6 4 5 3)" `shouldBe` Boolean False 

    describe "should compare if it is smaller" $ do
      it "when it is" $
        interpret "(< 2 4 6)" `shouldBe` Boolean True

      it "when it is not" $
        interpret "(< 6 4 2)" `shouldBe` Boolean False 

      it "when there is a wrong number in the sequence" $
        interpret "(> 3 5 4 6)" `shouldBe` Boolean False 

    describe "should compare if it is smaller than or equal" $ do
      it "when it is smaller" $
        interpret "(<= 2 4 6)" `shouldBe` Boolean True 

      it "when it is equal" $
        interpret "(<= 4 4 4)" `shouldBe` Boolean True 

      it "when it is not" $
        interpret "(<= 6 4 2)" `shouldBe` Boolean False 

      it "when there is a wrong number in the sequence" $
        interpret "(<= 3 5 4 6)" `shouldBe` Boolean False 

    describe "should define" $ do
      it "a number" $
        let newEnv = interpretAndGetEnv "(def five 5.0)"
         in interpretWithEnv newEnv "(+ 5 five)" `shouldBe` Number 10.0

      it "a lambda" $
        let newEnv = interpretAndGetEnv "(def add-one (fn (x) (+ 1 x)))"
         in interpretWithEnv newEnv "(add-one 5)" `shouldBe` Number 6.0

#ifndef LITTLE_AST_H
#define LITTLE_AST_H

namespace little {
  class Node {
  public :
   // void dump(std::ostream& out, int indent) = 0;
  };

  class Expr : public Node {

  };

  class IntLiteralExpr : public Expr {
  public :
    IntLiteralExpr(int value_) : value(value_) {}; 
  private :
    int value;
  };

  class BoolLiteralExpr : public Expr {
  public :
    BoolLiteralExpr(bool value_) : value(value_) {};
  private:
  	bool value;
  };

  class BinaryArithOperation : public Expr {
  public:
    enum class ArithOperator {
      plus,
      minus,
      mult,
      div
    };
  	BinaryArithOperation(ArithOperator op_, Expr* a_, Expr* b_):
  	  op(op_), a(a_), b(b_) {}

  private:
  	ArithOperator op;
  	Expr* a;
  	Expr* b;
  };

  class Stmt : public Node {
  };

  class FunctionDefStmt : public Stmt {

  };

  class AssignStmt : public Stmt {

  };

  class IfStmt : public Stmt {

  };

  class WhileStmt : public Stmt {

  };

  class RetStmt : public Stmt {

  };
}
#endif
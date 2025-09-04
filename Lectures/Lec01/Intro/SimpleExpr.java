// File Intro/SimpleExpr.java
// Java representation of expressions as in lecture 1
// sestoft@itu.dk * 2010-08-29

import java.util.Map;
import java.util.HashMap;

abstract class Expr { 
  abstract public int eval(Map<String,Integer> env);
  abstract public String fmt();
  abstract public String fmt2(Map<String,Integer> env);
  abstract public Expr simplify();
}

class CstI extends Expr { 
  protected final int i;

  public CstI(int i) { 
    this.i = i; 
  }

  public int eval(Map<String,Integer> env) {
    return i;
  } 

  public String fmt() {
    return ""+i;
  }

  public String fmt2(Map<String,Integer> env) {
    return ""+i;
  }

    // 1.4.4
    @Override
    public Expr simplify() {
        return this;
    }


}

class Var extends Expr { 
  protected final String name;

  public Var(String name) { 
    this.name = name; 
  }

  public int eval(Map<String,Integer> env) {
    return env.get(name);
  } 

  public String fmt() {
    return name;
  } 

  public String fmt2(Map<String,Integer> env) {
    return ""+env.get(name);
  }

    @Override
    public Expr simplify() {
        return this;
    }

}

class Prim extends Expr { 
  protected final String oper;
  protected final Expr e1, e2;

  public Prim(String oper, Expr e1, Expr e2) { 
    this.oper = oper; this.e1 = e1; this.e2 = e2;
  }

  public int eval(Map<String,Integer> env) {
    if (oper.equals("+"))
      return e1.eval(env) + e2.eval(env);
    else if (oper.equals("*"))
      return e1.eval(env) * e2.eval(env);
    else if (oper.equals("-"))
      return e1.eval(env) - e2.eval(env);
    else
      throw new RuntimeException("unknown primitive");
  } 

  public String fmt() {
    return "(" + e1.fmt() + oper + e2.fmt() + ")";
  } 

  public String fmt2(Map<String,Integer> env) {
    return "(" + e1.fmt2(env) + oper + e2.fmt2(env) + ")";
  }

    @Override
    public Expr simplify() {
        return null;
    }


}
/* 1.4.1 && 1.4.3 1.4.4*/
abstract class Binop extends Prim {
    public Binop(String oper, Expr e1, Expr e2) {
        super(oper, e1, e2);
    }
    public String toString() {
        return "(" + e1.fmt() + oper +  e2.fmt() + ")";
    }
}

class Add extends Binop {

    public Add(Expr e1, Expr e2) {
        super("+", e1, e2);
    }

    @Override
    public Expr simplify() {

        if (e1 instanceof CstI && e2 instanceof CstI) {
            if(((CstI) e1).i == 0)
                return e2.simplify();
            else if(((CstI) e2).i == 0)
                return e1.simplify();
        } else if (e1 instanceof CstI) {
            if (((CstI) e1).i == 0)
                return e2.simplify();
        } else if (e2 instanceof CstI) {
            if (((CstI) e2).i == 0)
                return e1.simplify();
        }
        return new Add(e1.simplify(), e2.simplify());


    }
}

class Sub extends Binop {

    public Sub(Expr e1, Expr e2) {
        super("-", e1, e2);
    }

    @Override
    public Expr simplify() {
        if (e2 instanceof CstI) {
            if (((CstI) e2).i == 0)
                return e1.simplify();
        }
        if (e1 instanceof Var && e2 instanceof Var) {
            if(((Var) e1).name.equals(((Var) e2).name))
                return new CstI(0);
        }
        return new Sub(e1.simplify(), e2.simplify());
    }
}

class Mul extends Binop {

    public Mul(Expr e1, Expr e2) {
        super("*", e1, e2);
    }

    @Override
    public Expr simplify() {
        if (e1 instanceof CstI) {
            if (((CstI) e1).i == 1)
                return e2.simplify();
            else if (((CstI) e1).i == 0)
                return new CstI(0);
        } else if (e2 instanceof CstI) {
            if (((CstI) e2).i == 1)
                return e1.simplify();
            else if (((CstI) e2).i == 0)
                return new CstI(0);
        }
        return new Mul(e1.simplify(), e2.simplify());
    }
}


public class SimpleExpr {
  public static void main(String[] args) {
    Expr e1 = new CstI(17);
    Expr e2 = new Prim("+", new CstI(3), new Var("a"));
    Expr e3 = new Prim("+", new Prim("*", new Var("b"), new CstI(9)), 
		            new Var("a"));
    Map<String,Integer> env0 = new HashMap<String,Integer>();
    env0.put("a", 3);
    env0.put("c", 78);
    env0.put("baf", 666);
    env0.put("b", 111);

    System.out.println("Env: " + env0.toString());

    /*1.4.2 */
    System.out.println(new Add(e1, e2));
    System.out.println(new Sub(e1, e2));
    System.out.println(new Mul(e1, e2));


    System.out.println(new Add(new Add(new Var("x"), new CstI(0)), new CstI(0)).simplify().fmt());
    System.out.println(new Sub(new Var("x"), new Var("y")).simplify().fmt());
    System.out.println(new Mul(new Var("x"), new CstI(1)).simplify().fmt());

    System.out.println(e1.fmt() + " = " + e1.fmt2(env0) + " = " + e1.eval(env0));
    System.out.println(e2.fmt() + " = " + e2.fmt2(env0) + " = " + e2.eval(env0));
    System.out.println(e3.fmt() + " = " + e3.fmt2(env0) + " = " + e3.eval(env0));
  }
}

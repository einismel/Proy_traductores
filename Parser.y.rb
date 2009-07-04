class RubyCalcParser

  prechigh
    nonassoc UMINUS AUX 
    left TkTimes TkDiv TkRes
    left TkPlus TkMinus
    nonassoc TkGE TkLE TkLess TkGreat TkDif TkSet 
    left TkDisy TkConj TkNeg
    nonassoc VACIO1
  preclow

  token UMINUS AUX VACIO VACIO1 TkPlus TkMinus TkTimes TkDiv TkSet TkDisy TkConj TkNeg TkRes TkLess TkGreat TkLE TkGE TkLength TkAP TkCP TkAC TkCC TkComa TkPP TkAsigD TkAsigI TkPC TkArrayOf TkNum TkStr TkMain TkOut TkIn TkBegin TkEnd TkProc TkAs TkReturn TkShow TkTrue TkFalse TkValue TkVar TkSkip TkIf TkFi TkDo TkOd TkId TkDif UX

rule

inicio : dec procedimiento ppal   { puts "dec procedimiento ppal" }
;

dec : dec TkVar ID TkPP tipo      { puts "dec -> dec var TkId("+ val[2] +"): tipo\n" }
    |                             { puts "dec -> lambda" }   
;

ID: ID TkComa TkId                { puts "ID -> ID , TkId("+ val[2] +") \n" } 
| TkId                            { puts "ID -> TkId(" + val[0] + ")\n" }
;

tipo: TkValue                     { puts "tipo -> value\n" } 
| TkArrayOf TkNum                 { puts "tipo -> Array of TkNum[" + val[1] + "]\n" } 
;

procedimiento : procedimiento TkProc TkId TkAP z TkCP TkAs dec instsp { puts "procedimiento -> procedimiento proc TkId("+val[3]+") ( z ) as dec instsp\n" }
              |                                                       { puts "dec -> lambda" }
;

z : z TkComa modo TkId TkComa     { puts "z -> z , modo TkId("+ val[3] +"), \n" }
  | modo TkId                     { puts "modo TkId(" + val[1] + ") \n" }
;

modo : TkIn                        { puts "modo -> TkIn\n" } 
     | TkOut                       { puts "modo -> TkOut\n" }
;

instsp: insts                      { puts "instsp -> insts\n" } 
      | TkReturn                   { puts "instsp -> TkReturn\n" } 
;

invocar: TkId TkAP w TkCP          { puts "invocar -> TkId(" + val[0] + ") ( w ) \n"}
;

w : w TkComa exp                    { puts " w -> w , exp\n" }  
  | exp                             { puts "w -> exp"}
;

mostrar : TkShow auxmostrar    
;

auxmostrar:  exp               =VACIO1     { puts "mostrar -> show exp" }
          |  TkStr             
;

ppal: TkMain instrucciones TkEnd        { puts "ppal -> main instrucciones end\n" }
;

instrucciones: instrucciones TkPC insts { puts "ppal -> instrucciones ; insts \n" }
| insts                                 { puts "instrucciones -> insts\n" }
;

insts: TkSkip                           { puts "insts -> skip\n" }
|  seleccion                            { puts "insts -> seleccion\n" }
|  asignacion                           { puts "insts -> asignacion\n"}
|  repeticion                           { puts "insts -> repeticion\n"}
|  bloque                               { puts "insts -> bloque\n"}
|  invocar                              { puts "insts -> invocar\n"}
|  mostrar                              { puts "insts -> mostrar\n"}
;

seleccion:  TkIf y TkFi                 { puts "seleccion -> if y fi\n" }
;

y: y ifaux                              
| ifaux         
;

ifaux: guardia TkAsigD insts            { puts "ifaux -> guardia <- insts\n"}
; 

asignacion: x TkComa asignacion TkComa exp =VACIO1   { puts "asignacion -> x asignacion , exp" } 
|  x TkAsigI exp                           =VACIO1   { puts "asignacion -> TkId(" +val[0] + ") <-  exp" } 
;

x: TkId                          { puts "x -> TkId[" + val[0] + "] ,\n" } 
|  TkId TkAC exp TkCC            { puts "x -> TkId (exp) ,\n" } 
;

repeticion: TkDo y TkOd                 { puts "repeticion -> do y od \n" }
;

bloque: TkBegin instrucciones TkEnd     { puts "begin instrucciones end" }
;

exp : exp TkPlus exp       { puts "exp -> exp + exp\n" }
    | exp TkMinus exp      { puts "exp -> exp - exp\n" }
    | exp TkTimes exp      { puts "exp -> exp * exp\n" }
    | exp TkDiv exp        { puts "exp -> exp / exp\n" }
    | exp TkRes exp        { puts "exp -> exp % exp\n" }
    | TkMinus exp        =UMINUS  { puts "exp -> - exp\n" } 
    | TkAP exp TkCP        { puts "exp -> ( exp )\n" }
    | TkNum                { puts "exp -> TkNum("+val[0]+")\n" }
    | TkId                 { puts "exp -> TkId("+val[0]+")\n" }
    | TkId TkAC exp TkCC   { puts "exp -> TkId("+val[0]+")[exp]\n" }
    | TkLength TkId        { puts "exp -> $ TkId("+val[0]+")\n" }
;

guardia : exp operador exp            { puts "guardia -> exp operador exp" }
        | guardia conector booleano   { puts "guardia -> guardia conector booleano" }
;

booleano: TkNeg guardia               { puts "booleano -> ~ guardia" }
        | TkTrue                      { puts "booleano -> true" }
        | TkFalse                     { puts "booleano -> false" }
;

operador: TkLE                        { puts "operador -> <="} 
        | TkGE                        { puts "operador -> >="} 
        | TkSet                       { puts "operador -> ="} 
        | TkDif                       { puts "operador -> !="} 
        | TkLess                      { puts "operador -> <"} 
        | TkGreat                     { puts "operador -> >"} 
;

conector: TkConj                      { puts "conector -> &&"} 
        | TkDisy                      { puts "conector -> ||"} 

;

end # RubyCalcParser

---- header ----
require 'RubyCalcAst'
require 'Token'
require 'excepciones'

---- inner ----
def initialize
@ast = nil
end

attr_accessor :lexer ;

  def parse()
   do_parse()
  end

  protected

  def next_token
    begin
      tok = @lexer.yylex
      return [false, false] if tok.nil?
      return [:"#{tok.class.to_s}",tok.value]
    rescue CaracterInesperado => err
      puts err
      next_token
    end
  end

  def on_error(t, val, vstack)
    raise ParseError, sprintf("\nError sintactico en el valor %s (%s)", val.inspect, token_to_str(t) || '?')
  end




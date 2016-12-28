/*
 * This file is part of OpenModelica.
 *
 * Copyright (c) 1998-2014, Open Source Modelica Consortium (OSMC),
 * c/o Linköpings universitet, Department of Computer and Information Science,
 * SE-58183 Linköping, Sweden.
 *
 * All rights reserved.
 *
 * THIS PROGRAM IS PROVIDED UNDER THE TERMS OF GPL VERSION 3 LICENSE OR
 * THIS OSMC PUBLIC LICENSE (OSMC-PL) VERSION 1.2.
 * ANY USE, REPRODUCTION OR DISTRIBUTION OF THIS PROGRAM CONSTITUTES
 * RECIPIENT'S ACCEPTANCE OF THE OSMC PUBLIC LICENSE OR THE GPL VERSION 3,
 * ACCORDING TO RECIPIENTS CHOICE.
 *
 * The OpenModelica software and the Open Source Modelica
 * Consortium (OSMC) Public License (OSMC-PL) are obtained
 * from OSMC, either from the above address,
 * from the URLs: http://www.ida.liu.se/projects/OpenModelica or
 * http://www.openmodelica.org, and in the OpenModelica distribution.
 * GNU version 3 is obtained from: http://www.gnu.org/copyleft/gpl.html.
 *
 * This program is distributed WITHOUT ANY WARRANTY; without
 * even the implied warranty of  MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE, EXCEPT AS EXPRESSLY SET FORTH
 * IN THE BY RECIPIENT SELECTED SUBSIDIARY LICENSE CONDITIONS OF OSMC-PL.
 *
 * See the full OSMC Public License conditions for more details.
 *
 */


encapsulated package NFDimension
" file:        NFMod.mo
  package:     NFMod
  description: A type for dimensions in NFInst.
"

public
import Absyn;
import DAE;
import NFExpression.Expression;

protected
import List;
import ExpressionDump;

public
type Dimensions = list<Dimension>;

uniontype Dimension
  record UNTYPED
    Absyn.Exp dimension;
    Boolean isProcessing;
  end UNTYPED;

  record TYPED
    Expression dimension;
  end TYPED;

  record WHOLE
  end WHOLE;

public
  function makeIntDim
    input Integer idim;
    output Dimension dim;
  algorithm
    dim := TYPED(Expression.INTEGER(idim));
  end makeIntDim;

  function dimensionsKnownAndEqual
    "Checks that two dimensions are specified and equal."
    input Dimension dim1;
    input Dimension dim2;
    output Boolean res;
  algorithm
    res := match (dim1,dim2)
      case (WHOLE(), _) then false; // TODO! FIXME! is this correct??
      case (_, WHOLE()) then false; // TODO! FIXME! is this correct??
      else valueEq(dim1, dim2);
    end match;
  end dimensionsKnownAndEqual;

  function dimensionsEqual
    "Checks that two dimensions are specified and equal."
    input Dimension dim1;
    input Dimension dim2;
    output Boolean res;
  algorithm
    res := valueEq(dim1, dim2);
  end dimensionsEqual;

  function toDAEDim
    input Dimension dim;
    output DAE.Dimension daeDim;
  algorithm
    daeDim := match dim
      local
        Expression e;

      case TYPED(dimension = e as Expression.INTEGER())
        then DAE.DIM_INTEGER(e.value);

      case TYPED(dimension = e)
        then DAE.DIM_EXP(Expression.toDAEExp(e));

      case WHOLE() then DAE.DIM_UNKNOWN();
    end match;
  end toDAEDim;

  function toString
    input Dimension d;
    output String s;
  algorithm
    s := ExpressionDump.dimensionString(toDAEDim(d));
  end toString;
end Dimension;

annotation(__OpenModelica_Interface="frontend");
end NFDimension;

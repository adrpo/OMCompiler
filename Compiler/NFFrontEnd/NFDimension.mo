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

public
uniontype Dimension
  record UNTYPED_DIM
    Absyn.Exp dimension;
    Boolean isProcessing;
  end UNTYPED_DIM;

  record TYPED_DIM
    Expression dimension;
  end TYPED_DIM;

  record WHOLE_DIM
  end WHOLE_DIM;
public
  function makeIntDim
    input Integer idim;
    output Dimension dim;
  algorithm
    dim := TYPED_DIM(Expression.INTEGER(idim));
  end makeIntDim;

  function toDAEDim
    input Dimension dim;
    output DAE.Dimension daeDim;
  algorithm
    daeDim := match dim
      local
        Expression e;

      case TYPED_DIM(dimension = e as Expression.INTEGER())
        then DAE.DIM_INTEGER(e.value);

      case TYPED_DIM(dimension = e) then DAE.DIM_EXP(Expression.toDAEExp(e));
      case WHOLE_DIM() then DAE.DIM_UNKNOWN();
    end match;
  end toDAEDim;
end Dimension;

annotation(__OpenModelica_Interface="frontend");
end NFDimension;

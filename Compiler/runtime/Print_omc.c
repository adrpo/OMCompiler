/*
 * This file is part of OpenModelica.
 *
 * Copyright (c) 1998-2010, Linköpings University,
 * Department of Computer and Information Science,
 * SE-58183 Linköping, Sweden.
 *
 * All rights reserved.
 *
 * THIS PROGRAM IS PROVIDED UNDER THE TERMS OF THIS OSMC PUBLIC
 * LICENSE (OSMC-PL). ANY USE, REPRODUCTION OR DISTRIBUTION OF
 * THIS PROGRAM CONSTITUTES RECIPIENT'S ACCEPTANCE OF THE OSMC
 * PUBLIC LICENSE.
 *
 * The OpenModelica software and the Open Source Modelica
 * Consortium (OSMC) Public License (OSMC-PL) are obtained
 * from Linköpings University, either from the above address,
 * from the URL: http://www.ida.liu.se/projects/OpenModelica
 * and in the OpenModelica distribution.
 *
 * This program is distributed  WITHOUT ANY WARRANTY; without
 * even the implied warranty of  MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE, EXCEPT AS EXPRESSLY SET FORTH
 * IN THE BY RECIPIENT SELECTED SUBSIDIARY LICENSE CONDITIONS
 * OF OSMC-PL.
 *
 * See the full OSMC Public License conditions for more details.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include "meta_modelica.h"

#include "printimpl.c"
#include "ModelicaUtilities.h"

extern int Print_saveAndClearBuf(threadData_t *threadData)
{
  int handle = PrintImpl__saveAndClearBuf();
  if (handle < 0) MMC_THROW();
  return handle;
}

extern void Print_restoreBuf(threadData_t *threadData,int handle)
{
  if (PrintImpl__restoreBuf(handle))
    MMC_THROW();
}

extern void Print_printErrorBuf(threadData_t *threadData,const char* str)
{
  if (PrintImpl__printErrorBuf(str))
    MMC_THROW();
}

extern void Print_printBuf(threadData_t *threadData,const char* str)
{
  // fprintf(stderr, "Print_printBuf: %s\n", str);
  if (PrintImpl__printBuf(str))
    MMC_THROW();
}

extern int Print_hasBufNewLineAtEnd(threadData_t *threadData)
{
  return PrintImpl__hasBufNewLineAtEnd();
}

extern int Print_getBufLength(threadData_t *threadData)
{
  return PrintImpl__getBufLength();
}

extern const char* Print_getString(threadData_t *threadData)
{
  const char* res = PrintImpl__getString();
  if (res == NULL)
    MMC_THROW();
  // fprintf(stderr, "Print_getString: %s##\n", res);fflush(NULL);
  return strcpy(ModelicaAllocateString(strlen(res)), res);
}

extern const char* Print_getErrorString(threadData_t *threadData)
{
  const char* res = PrintImpl__getErrorString();
  if (res == NULL)
    MMC_THROW();
  return strcpy(ModelicaAllocateString(strlen(res)), res);
}

extern void Print_clearErrorBuf(threadData_t *threadData)
{
  PrintImpl__clearErrorBuf();
}

extern void Print_clearBuf(threadData_t *threadData)
{
  // fprintf(stderr, "Print_clearBuf\n");
  PrintImpl__clearBuf();
}

extern void Print_printBufSpace(threadData_t *threadData,int numSpace)
{
  if (PrintImpl__printBufSpace(numSpace))
    MMC_THROW();
}

extern void Print_printBufNewLine(threadData_t *threadData)
{
  if (PrintImpl__printBufNewLine())
    MMC_THROW();
}

extern void Print_writeBuf(threadData_t *threadData,const char* filename)
{
  if (PrintImpl__writeBuf(filename))
    MMC_THROW();
}

extern void Print_writeBufConvertLines(threadData_t *threadData,const char* filename)
{
  if (PrintImpl__writeBufConvertLines(filename))
    MMC_THROW();
}

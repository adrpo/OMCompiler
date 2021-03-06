static inline void *mmc_mk_box0(unsigned int ctor)
{
  struct mmc_struct *p = (struct mmc_struct *) mmc_alloc_words(1);
  p->header = MMC_STRUCTHDR(0, ctor);
  return MMC_TAGPTR(p);
}
static inline void *mmc_mk_box1(unsigned int ctor, void *x0)
{
  struct mmc_struct *p = (struct mmc_struct *) mmc_alloc_words(2);
  void **data = p->data;
  p->header = MMC_STRUCTHDR(1, ctor);
  data[0] = (void*) x0;
  return MMC_TAGPTR(p);
}
static inline void *mmc_mk_box2(unsigned int ctor, void *x0, void *x1)
{
  struct mmc_struct *p = (struct mmc_struct *) mmc_alloc_words(3);
  void **data = p->data;
  p->header = MMC_STRUCTHDR(2, ctor);
  data[0] = (void*) x0;
  data[1] = (void*) x1;
  return MMC_TAGPTR(p);
}
static inline void *mmc_mk_box3(unsigned int ctor, void *x0, void *x1, void *x2)
{
  struct mmc_struct *p = (struct mmc_struct *) mmc_alloc_words(4);
  void **data = p->data;
  p->header = MMC_STRUCTHDR(3, ctor);
  data[0] = (void*) x0;
  data[1] = (void*) x1;
  data[2] = (void*) x2;
  return MMC_TAGPTR(p);
}
static inline void *mmc_mk_box4(unsigned int ctor, void *x0, void *x1, void *x2, void *x3)
{
  struct mmc_struct *p = (struct mmc_struct *) mmc_alloc_words(5);
  void **data = p->data;
  p->header = MMC_STRUCTHDR(4, ctor);
  data[0] = (void*) x0;
  data[1] = (void*) x1;
  data[2] = (void*) x2;
  data[3] = (void*) x3;
  return MMC_TAGPTR(p);
}
static inline void *mmc_mk_box5(unsigned int ctor, void *x0, void *x1, void *x2, void *x3, void *x4)
{
  struct mmc_struct *p = (struct mmc_struct *) mmc_alloc_words(6);
  void **data = p->data;
  p->header = MMC_STRUCTHDR(5, ctor);
  data[0] = (void*) x0;
  data[1] = (void*) x1;
  data[2] = (void*) x2;
  data[3] = (void*) x3;
  data[4] = (void*) x4;
  return MMC_TAGPTR(p);
}
static inline void *mmc_mk_box6(unsigned int ctor, void *x0, void *x1, void *x2, void *x3, void *x4, void *x5)
{
  struct mmc_struct *p = (struct mmc_struct *) mmc_alloc_words(7);
  void **data = p->data;
  p->header = MMC_STRUCTHDR(6, ctor);
  data[0] = (void*) x0;
  data[1] = (void*) x1;
  data[2] = (void*) x2;
  data[3] = (void*) x3;
  data[4] = (void*) x4;
  data[5] = (void*) x5;
  return MMC_TAGPTR(p);
}
static inline void *mmc_mk_box7(unsigned int ctor, void *x0, void *x1, void *x2, void *x3, void *x4, void *x5, void *x6)
{
  struct mmc_struct *p = (struct mmc_struct *) mmc_alloc_words(8);
  void **data = p->data;
  p->header = MMC_STRUCTHDR(7, ctor);
  data[0] = (void*) x0;
  data[1] = (void*) x1;
  data[2] = (void*) x2;
  data[3] = (void*) x3;
  data[4] = (void*) x4;
  data[5] = (void*) x5;
  data[6] = (void*) x6;
  return MMC_TAGPTR(p);
}
static inline void *mmc_mk_box8(unsigned int ctor, void *x0, void *x1, void *x2, void *x3, void *x4, void *x5, void *x6, void *x7)
{
  struct mmc_struct *p = (struct mmc_struct *) mmc_alloc_words(9);
  void **data = p->data;
  p->header = MMC_STRUCTHDR(8, ctor);
  data[0] = (void*) x0;
  data[1] = (void*) x1;
  data[2] = (void*) x2;
  data[3] = (void*) x3;
  data[4] = (void*) x4;
  data[5] = (void*) x5;
  data[6] = (void*) x6;
  data[7] = (void*) x7;
  return MMC_TAGPTR(p);
}
static inline void *mmc_mk_box9(unsigned int ctor, void *x0, void *x1, void *x2, void *x3, void *x4, void *x5, void *x6, void *x7, void *x8)
{
  struct mmc_struct *p = (struct mmc_struct *) mmc_alloc_words(10);
  void **data = p->data;
  p->header = MMC_STRUCTHDR(9, ctor);
  data[0] = (void*) x0;
  data[1] = (void*) x1;
  data[2] = (void*) x2;
  data[3] = (void*) x3;
  data[4] = (void*) x4;
  data[5] = (void*) x5;
  data[6] = (void*) x6;
  data[7] = (void*) x7;
  data[8] = (void*) x8;
  return MMC_TAGPTR(p);
}
static inline void *mmc_mk_box10(unsigned int ctor, void *x0, void *x1, void *x2, void *x3, void *x4, void *x5, void *x6, void *x7, void *x8, void *x9)
{
  struct mmc_struct *p = (struct mmc_struct *) mmc_alloc_words(11);
  void **data = p->data;
  p->header = MMC_STRUCTHDR(10, ctor);
  data[0] = (void*) x0;
  data[1] = (void*) x1;
  data[2] = (void*) x2;
  data[3] = (void*) x3;
  data[4] = (void*) x4;
  data[5] = (void*) x5;
  data[6] = (void*) x6;
  data[7] = (void*) x7;
  data[8] = (void*) x8;
  data[9] = (void*) x9;
  return MMC_TAGPTR(p);
}
static inline void *mmc_mk_box11(unsigned int ctor, void *x0, void *x1, void *x2, void *x3, void *x4, void *x5, void *x6, void *x7, void *x8, void *x9, void *x10)
{
  struct mmc_struct *p = (struct mmc_struct *) mmc_alloc_words(12);
  void **data = p->data;
  p->header = MMC_STRUCTHDR(11, ctor);
  data[0] = (void*) x0;
  data[1] = (void*) x1;
  data[2] = (void*) x2;
  data[3] = (void*) x3;
  data[4] = (void*) x4;
  data[5] = (void*) x5;
  data[6] = (void*) x6;
  data[7] = (void*) x7;
  data[8] = (void*) x8;
  data[9] = (void*) x9;
  data[10] = (void*) x10;
  return MMC_TAGPTR(p);
}
static inline void *mmc_mk_box12(unsigned int ctor, void *x0, void *x1, void *x2, void *x3, void *x4, void *x5, void *x6, void *x7, void *x8, void *x9, void *x10, void *x11)
{
  struct mmc_struct *p = (struct mmc_struct *) mmc_alloc_words(13);
  void **data = p->data;
  p->header = MMC_STRUCTHDR(12, ctor);
  data[0] = (void*) x0;
  data[1] = (void*) x1;
  data[2] = (void*) x2;
  data[3] = (void*) x3;
  data[4] = (void*) x4;
  data[5] = (void*) x5;
  data[6] = (void*) x6;
  data[7] = (void*) x7;
  data[8] = (void*) x8;
  data[9] = (void*) x9;
  data[10] = (void*) x10;
  data[11] = (void*) x11;
  return MMC_TAGPTR(p);
}
static inline void *mmc_mk_box13(unsigned int ctor, void *x0, void *x1, void *x2, void *x3, void *x4, void *x5, void *x6, void *x7, void *x8, void *x9, void *x10, void *x11, void *x12)
{
  struct mmc_struct *p = (struct mmc_struct *) mmc_alloc_words(14);
  void **data = p->data;
  p->header = MMC_STRUCTHDR(13, ctor);
  data[0] = (void*) x0;
  data[1] = (void*) x1;
  data[2] = (void*) x2;
  data[3] = (void*) x3;
  data[4] = (void*) x4;
  data[5] = (void*) x5;
  data[6] = (void*) x6;
  data[7] = (void*) x7;
  data[8] = (void*) x8;
  data[9] = (void*) x9;
  data[10] = (void*) x10;
  data[11] = (void*) x11;
  data[12] = (void*) x12;
  return MMC_TAGPTR(p);
}
static inline void *mmc_mk_box14(unsigned int ctor, void *x0, void *x1, void *x2, void *x3, void *x4, void *x5, void *x6, void *x7, void *x8, void *x9, void *x10, void *x11, void *x12, void *x13)
{
  struct mmc_struct *p = (struct mmc_struct *) mmc_alloc_words(15);
  void **data = p->data;
  p->header = MMC_STRUCTHDR(14, ctor);
  data[0] = (void*) x0;
  data[1] = (void*) x1;
  data[2] = (void*) x2;
  data[3] = (void*) x3;
  data[4] = (void*) x4;
  data[5] = (void*) x5;
  data[6] = (void*) x6;
  data[7] = (void*) x7;
  data[8] = (void*) x8;
  data[9] = (void*) x9;
  data[10] = (void*) x10;
  data[11] = (void*) x11;
  data[12] = (void*) x12;
  data[13] = (void*) x13;
  return MMC_TAGPTR(p);
}
static inline void *mmc_mk_box15(unsigned int ctor, void *x0, void *x1, void *x2, void *x3, void *x4, void *x5, void *x6, void *x7, void *x8, void *x9, void *x10, void *x11, void *x12, void *x13, void *x14)
{
  struct mmc_struct *p = (struct mmc_struct *) mmc_alloc_words(16);
  void **data = p->data;
  p->header = MMC_STRUCTHDR(15, ctor);
  data[0] = (void*) x0;
  data[1] = (void*) x1;
  data[2] = (void*) x2;
  data[3] = (void*) x3;
  data[4] = (void*) x4;
  data[5] = (void*) x5;
  data[6] = (void*) x6;
  data[7] = (void*) x7;
  data[8] = (void*) x8;
  data[9] = (void*) x9;
  data[10] = (void*) x10;
  data[11] = (void*) x11;
  data[12] = (void*) x12;
  data[13] = (void*) x13;
  data[14] = (void*) x14;
  return MMC_TAGPTR(p);
}
static inline void *mmc_mk_box16(unsigned int ctor, void *x0, void *x1, void *x2, void *x3, void *x4, void *x5, void *x6, void *x7, void *x8, void *x9, void *x10, void *x11, void *x12, void *x13, void *x14, void *x15)
{
  struct mmc_struct *p = (struct mmc_struct *) mmc_alloc_words(17);
  void **data = p->data;
  p->header = MMC_STRUCTHDR(16, ctor);
  data[0] = (void*) x0;
  data[1] = (void*) x1;
  data[2] = (void*) x2;
  data[3] = (void*) x3;
  data[4] = (void*) x4;
  data[5] = (void*) x5;
  data[6] = (void*) x6;
  data[7] = (void*) x7;
  data[8] = (void*) x8;
  data[9] = (void*) x9;
  data[10] = (void*) x10;
  data[11] = (void*) x11;
  data[12] = (void*) x12;
  data[13] = (void*) x13;
  data[14] = (void*) x14;
  data[15] = (void*) x15;
  return MMC_TAGPTR(p);
}
static inline void *mmc_mk_box17(unsigned int ctor, void *x0, void *x1, void *x2, void *x3, void *x4, void *x5, void *x6, void *x7, void *x8, void *x9, void *x10, void *x11, void *x12, void *x13, void *x14, void *x15, void *x16)
{
  struct mmc_struct *p = (struct mmc_struct *) mmc_alloc_words(18);
  void **data = p->data;
  p->header = MMC_STRUCTHDR(17, ctor);
  data[0] = (void*) x0;
  data[1] = (void*) x1;
  data[2] = (void*) x2;
  data[3] = (void*) x3;
  data[4] = (void*) x4;
  data[5] = (void*) x5;
  data[6] = (void*) x6;
  data[7] = (void*) x7;
  data[8] = (void*) x8;
  data[9] = (void*) x9;
  data[10] = (void*) x10;
  data[11] = (void*) x11;
  data[12] = (void*) x12;
  data[13] = (void*) x13;
  data[14] = (void*) x14;
  data[15] = (void*) x15;
  data[16] = (void*) x16;
  return MMC_TAGPTR(p);
}
static inline void *mmc_mk_box18(unsigned int ctor, void *x0, void *x1, void *x2, void *x3, void *x4, void *x5, void *x6, void *x7, void *x8, void *x9, void *x10, void *x11, void *x12, void *x13, void *x14, void *x15, void *x16, void *x17)
{
  struct mmc_struct *p = (struct mmc_struct *) mmc_alloc_words(19);
  void **data = p->data;
  p->header = MMC_STRUCTHDR(18, ctor);
  data[0] = (void*) x0;
  data[1] = (void*) x1;
  data[2] = (void*) x2;
  data[3] = (void*) x3;
  data[4] = (void*) x4;
  data[5] = (void*) x5;
  data[6] = (void*) x6;
  data[7] = (void*) x7;
  data[8] = (void*) x8;
  data[9] = (void*) x9;
  data[10] = (void*) x10;
  data[11] = (void*) x11;
  data[12] = (void*) x12;
  data[13] = (void*) x13;
  data[14] = (void*) x14;
  data[15] = (void*) x15;
  data[16] = (void*) x16;
  data[17] = (void*) x17;
  return MMC_TAGPTR(p);
}
static inline void *mmc_mk_box19(unsigned int ctor, void *x0, void *x1, void *x2, void *x3, void *x4, void *x5, void *x6, void *x7, void *x8, void *x9, void *x10, void *x11, void *x12, void *x13, void *x14, void *x15, void *x16, void *x17, void *x18)
{
  struct mmc_struct *p = (struct mmc_struct *) mmc_alloc_words(20);
  void **data = p->data;
  p->header = MMC_STRUCTHDR(19, ctor);
  data[0] = (void*) x0;
  data[1] = (void*) x1;
  data[2] = (void*) x2;
  data[3] = (void*) x3;
  data[4] = (void*) x4;
  data[5] = (void*) x5;
  data[6] = (void*) x6;
  data[7] = (void*) x7;
  data[8] = (void*) x8;
  data[9] = (void*) x9;
  data[10] = (void*) x10;
  data[11] = (void*) x11;
  data[12] = (void*) x12;
  data[13] = (void*) x13;
  data[14] = (void*) x14;
  data[15] = (void*) x15;
  data[16] = (void*) x16;
  data[17] = (void*) x17;
  data[18] = (void*) x18;
  return MMC_TAGPTR(p);
}
static inline void *mmc_mk_box20(unsigned int ctor, void *x0, void *x1, void *x2, void *x3, void *x4, void *x5, void *x6, void *x7, void *x8, void *x9, void *x10, void *x11, void *x12, void *x13, void *x14, void *x15, void *x16, void *x17, void *x18, void *x19)
{
  struct mmc_struct *p = (struct mmc_struct *) mmc_alloc_words(21);
  void **data = p->data;
  p->header = MMC_STRUCTHDR(20, ctor);
  data[0] = (void*) x0;
  data[1] = (void*) x1;
  data[2] = (void*) x2;
  data[3] = (void*) x3;
  data[4] = (void*) x4;
  data[5] = (void*) x5;
  data[6] = (void*) x6;
  data[7] = (void*) x7;
  data[8] = (void*) x8;
  data[9] = (void*) x9;
  data[10] = (void*) x10;
  data[11] = (void*) x11;
  data[12] = (void*) x12;
  data[13] = (void*) x13;
  data[14] = (void*) x14;
  data[15] = (void*) x15;
  data[16] = (void*) x16;
  data[17] = (void*) x17;
  data[18] = (void*) x18;
  data[19] = (void*) x19;
  return MMC_TAGPTR(p);
}

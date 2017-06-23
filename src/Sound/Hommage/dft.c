#include <stdio.h>
// #include <io.h>
#include <fcntl.h>
#include <stdlib.h>
#include <math.h>


double sinus [1024];
double cosin [1024];
double kurve [1024];

int cinit ()
{
  int i;
  double f = 3.1415926 / 512.0;
  double k = 1.0 / 512.0;
  for (i=0; i<1024; i++)
  { double x = f * (double) i;
    sinus [i] = sin (x);
    cosin [i] = cos (x);
  }

  /*
  for (i=0; i<512; i++)
  { double x = k * (double) i;
    kurve [i] = x;
    kurve [i+512] = 1.0-x;
  }
  */

  for (i=0; i<1024; i++)
  { kurve [i] = 0.5 - (cosin [i] * 0.5);
  }

  return 0;
}

double getsinus (int x)
{ return sinus [x & 1023];
}
double getcosin (int x)
{ return cosin [x & 1023];
}

void xcanalyse (double * wv, double * cf)
{ int k, x;
  for (k=1; k<512; k++)
  { int kc = k << 1;
    int ks = kc - 1;
    cf [kc] = 0.0;
    cf [ks] = 0.0;
    for (x=0; x<1024; x++)
    { int i = (x * k) & 1023;
      cf [kc] += cosin [i] * wv [x];
      cf [ks] += sinus [i] * wv [x];
    }
    cf [kc] /= 512.0;
    cf [ks] /= 512.0;
  }
  cf [1023] = 0.0;
  cf [0] = 0.0;
  for (x=0; x<1024; x++)
  { if (x % 2 == 0)
      cf [1023] += wv [x];
    else
      cf [1023] -= wv [x];
    cf [0] += wv [x];
  }
  cf [0] /= 512.0;
  cf [1023] /= 512.0;
}

void xcsynthese (double * cf, double * wv)
{ int x,k;
  double d;
  for (x=0; x<1024; x++)
  { d = cf [0] * 0.5;
    if (x % 2 == 0)
      d += cf [1023];
    else
      d -= cf [1023];
    for (k=1; k<512; k++)
    { int kc = k << 1;
      int ks = kc - 1;
      int i = (x * k) & 1023;
      d += cosin [i] * cf [kc];
      d += sinus [i] * cf [ks];
    }
    wv [x] = d; // * st->kurve [x];
  }
}

void ckurve (double * arr)
{ int i;
  for (i=0; i<1024; i++)
    arr [i] *= kurve [i];
}

// Now with FFT ...

void canalyse (double * wv, double * cf)
{ int group_length;
  int i,j,d,nr_of_groups,group;
  int a_ix, b_ix, w_ix;

  int a_r_ix, a_i_ix, b_r_ix, b_i_ix;

  double a_r, a_i, b_r, b_i, w_r, w_i, wb_r, wb_i;

  group_length = 1;

  for (i=0; i<512; i++)
  { j = 0; //bitreverse (i);
    int ii = i;
    for (d=0;d<9;d++)
    { j<<=1;
      if (ii % 2)
      { j++;
      }
      ii >>=1;
    }
    cf [i*2] = wv [j*2];
    cf [i*2+1] = wv [j*2+1];
  }


  for (nr_of_groups=512; nr_of_groups>1; nr_of_groups>>=1)
  { 
    for (group=0; group<nr_of_groups; group+=2)
    { 
      for (i=0; i<group_length; i++) // zipWith3 butterfly ws xs ys
      { a_ix = group * group_length + i;
        b_ix = a_ix + group_length;

        w_ix = (i * 512 / group_length);

        //butterfly (w_ix, a_ix, b_ix);

        w_r = cosin [w_ix];
        w_i = sinus [w_ix];

        a_r_ix = a_ix * 2;
        a_i_ix = a_r_ix + 1;

        b_r_ix = b_ix * 2;
        b_i_ix = b_r_ix + 1;
        
        a_r = cf [a_r_ix];
        a_i = cf [a_i_ix];

        b_r = cf [b_r_ix];
        b_i = cf [b_i_ix];

        wb_r = (w_r * b_r) + (w_i * b_i);
        wb_i = (w_r * b_i) - (w_i * b_r);

        cf [a_r_ix] = a_r + wb_r;
        cf [a_i_ix] = a_i + wb_i;

        cf [b_r_ix] = a_r - wb_r;
        cf [b_i_ix] = a_i - wb_i;
        
      }
    }
    group_length *= 2;
  }
}

void csynthese (double * wv, double * cf)
{ int group_length;
  int i,j,d,nr_of_groups,group;
  int a_ix, b_ix, w_ix;
  int a_r_ix, a_i_ix, b_r_ix, b_i_ix;

  double a_r, a_i, b_r, b_i, w_r, w_i, wb_r, wb_i;

  group_length = 1;

  for (i=0; i<512; i++)
  { j = 0; //bitreverse (i);
    int ii = i;
    for (d=0;d<9;d++)
    { j<<=1;
      if (ii % 2)
      { j++;
      }
      ii >>=1;
    }
    cf [i*2] = wv [j*2];
    cf [i*2+1] = wv [j*2+1];
  }

  for (nr_of_groups=512; nr_of_groups>1; nr_of_groups>>=1)
  { 
    for (group=0; group<nr_of_groups; group+=2)
    { 
      for (i=0; i<group_length; i++) // zipWith3 butterfly ws xs ys
      { a_ix = group * group_length + i;
        b_ix = a_ix + group_length;

        w_ix = (i * 512 / group_length);

        //butterfly (w_ix, a_ix, b_ix);

        w_r = cosin [w_ix];
        w_i = sinus [w_ix];


        a_r_ix = a_ix * 2;
        a_i_ix = a_r_ix + 1;

        b_r_ix = b_ix * 2;
        b_i_ix = b_r_ix + 1;
        
        a_r = cf [a_r_ix];
        a_i = cf [a_i_ix];

        b_r = cf [b_r_ix];
        b_i = cf [b_i_ix];

        wb_r = (w_r * b_r) - (w_i * b_i);
        wb_i = (w_r * b_i) + (w_i * b_r);

        cf [a_r_ix] = (a_r + wb_r) * 0.5;
        cf [a_i_ix] = (a_i + wb_i) * 0.5;

        cf [b_r_ix] = (a_r - wb_r) * 0.5;
        cf [b_i_ix] = (a_i - wb_i) * 0.5;
        
      }
    }
    group_length *= 2;
  }
}


// a + w * b

// (ar,ai) +- (wr,wi) * (br,bi)
//            (wr*br - wi*bi, wr*bi+br*wi)
// (ar+wr*br-wi*bi, ai+wr*bi+br*wi)
// (ar-wr*br+wi*bi, ai-wr*bi+br*wi)

// 1024 i = cosin [i]
// 512 i = cosin [i*2]   [i * 1024 / grouplength]
// 2 i = i * 512 












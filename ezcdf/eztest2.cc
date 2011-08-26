#ifndef __eztest2__
#define __eztest2__

#include "ezcdf.hh"

/**
 * Sample program to read and write data using EZcdf, an easy to use
 * to read and write netCDF files.
 * This program is similar to the fortran eztest.f90 program, writing
 * the same file and reading the same data. Hence, the C++ version upon
 * which this program is based is compatible with the f90 version. Files
 * produced in fortran can be read from C++ and inversely. There are, 
 * however, some subtle differences; files are open through a constructor
 * call and closed through a destructor call. This distinction is important
 * as data can only be read once the file is closed, the reading and writing
 * must therefeore take place in different scopes. Another important 
 * difference is that C/C++ data are contiguous along rows so that in order
 * to produce the same netCDF file the order of sizes in dimlens should 
 * be opposite in the C++ version.
 * 
 * Compiler must be ISO/ANSI 97 compliant.
 *
 * pletzer@pppl.gov 
 *
 * Wed Apr 12 16:31:40 EDT 2000
 */

void sampleWrite(void);

void sampleRead(void);

int main(){

  // write data

  sampleWrite();
  
  // read data back
  
  sampleRead();

  return 0;
}

void sampleWrite(void){

  
  // open file

  EZcdf nc("EZtest2.cdf", "w");
  int dims[3] = {1,1,1};

  // define variables

  char title[24]="Test of EZcdf Interface";
  dims[0]=1; dims[1]=1; dims[2]=24;
  nc.cdfDefVar("Title", dims, "CHAR");

  char comment[2][9] = {"Written ","02/15/99"};
  dims[0]=1; dims[1]=2; dims[2]=9;
  nc.cdfDefVar("Comment", dims, "CHAR");

  double s = 999.99;
  dims[0]=1; dims[1]=1; dims[2]=1;
  nc.cdfDefVar("Scalar", dims, "R8");

  int ival[5] = {1,2,3,4,5};
  dims[0]=1; dims[1]=1; dims[2]=5;
  nc.cdfDefVar("1D-INT", dims, "INT");

  
  double dval[4][3]={
      {11.1,12.2,13.3},{21.1,22.2,23.3},{31.1,32.2,33.3},{41.1,43.2,43.3}
  };
  dims[0]=1; dims[1]=4; dims[2]=3;
  nc.cdfDefVar("2D-R8", dims, "R8");
  
  float fval[2][4][3]={
    {
      {1.1,1.2,1.3},{2.1, 2.2, 2.3},{3.1, 3.2, 3.3},{4.1, 4.2, 4.3}
    },
    {
      {5.1, 5.2, 5.3},{6.1, 6.2, 6.3},{7.1, 7.2, 7.3},{8.1, 8.2, 8.3}
    }
  };
  dims[0]=2; dims[1]=4; dims[2]=3;
  nc.cdfDefVar("3D-R4", dims, "R4");

  // write data
  
  nc.cdfPutVar("Title", &title[0]);
  nc.cdfPutVar("Comment", &comment[0][0]);
  nc.cdfPutVar("Scalar", &s);
  nc.cdfPutVar("1D-INT", &ival[0]);
  nc.cdfPutVar("2D-R8", &dval[0][0]);
  nc.cdfPutVar("3D-R4", &fval[0][0][0]);

  // file is closed in destructor

}

void sampleRead(void){

  // open file in read mode

  EZcdf nc2("EZtest2.cdf");

  int dimlens[CDF_MAXDIM];
  string xtype;

  // inquire about dimensions and type

  nc2.cdfInqVar("Title", dimlens, xtype);
  cout << "Title dims: ";
  int n_t=1, i;
  for (i=0; i<CDF_MAXDIM; i++) {
    cout << dimlens[i] << ' ';
    n_t *= dimlens[i];
  }
  cout << " of type " << xtype << endl;
  
  nc2.cdfInqVar("Comment", dimlens, xtype);
  cout << "Comment dims: ";
  int n_c=1;
  for (i=0; i<CDF_MAXDIM; i++) {
    cout << dimlens[i] << ' ';
    n_c *= dimlens[i];
  }
  cout << " of type " << xtype << endl;

  nc2.cdfInqVar("Scalar", dimlens, xtype);
  cout << "Scalar dims: ";
  int n_s=1;
  for (i=0; i<CDF_MAXDIM; i++) {
    cout << dimlens[i] << ' ';
    n_s *=dimlens[i];
  }
  cout << " of type " << xtype << endl;

  nc2.cdfInqVar("1D-INT", dimlens, xtype);
  cout << "1D-INT dims: ";
  int n_i=1;
  for (i=0; i<CDF_MAXDIM; i++) {
    cout << dimlens[i] << ' ';
    n_i *= dimlens[i];
  }
  cout << " of type " << xtype << endl;

  nc2.cdfInqVar("2D-R8", dimlens, xtype);
  cout << "2D-R8 dims: ";
  int n_d=1;
  for (i=0; i<CDF_MAXDIM; i++) {
    cout << dimlens[i] << ' ';
    n_d *= dimlens[i];
  }
  cout << " of type " << xtype << endl;

  nc2.cdfInqVar("3D-R4", dimlens, xtype);
  cout << "3D-R4 dims: ";
  int n_f=1;
  for (i=0; i<CDF_MAXDIM; i++) {
    cout << dimlens[i] << ' ';
    n_f *= dimlens[i];
  }
  cout << " of type " << xtype << endl;

  // read data

  char *title = new char[n_t+1];
  nc2.cdfGetVar("Title", title);
  char comment[2][10];
  nc2.cdfGetVar("Comment", &comment[0][0]);
  double s;
  nc2.cdfGetVar("Scalar", &s);
  int *ival= new int[n_i];
  nc2.cdfGetVar("1D-INT", ival);
  double *dval = new double[n_d];
  nc2.cdfGetVar("2D-R8", dval);
  float *fval = new float[n_f];
  nc2.cdfGetVar("3D-R4", fval);

  cout << "Title: " << title << endl;
  cout << "Comment: " << comment[0] << ' ' << comment[1] << endl;
  cout << "Scalar: " << s << endl;
  cout << "1D-INT: ";
  for(i=0; i<n_i; i++) cout << ival[i] << ' ';
  cout << endl;
  cout << "2D-R8: ";
  for(i=0; i<n_d; i++) cout << dval[i] << ' ';
  cout << endl;
  cout << "3D-R4: ";
  for(i=0; i<n_f; i++) cout << fval[i] << ' ';
  cout << endl;
  

  delete[] title;
  delete[] ival;
  delete[] dval;
  delete[] fval;

}
#endif // __eztest2__

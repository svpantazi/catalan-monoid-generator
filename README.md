# catalan-monoid-generator
Algorithm to generate all elements of Catalan monoids

Catalan Monoid Generator

File list:

	README.txt				this file
	
	license.txt				the GPL license file
	
	generate_catalan_monoid.pp		the main algorithm source file
	
	catalan_utils.pas			utility functions
	
	generate_catalan_monoid.lpi		Lazarus-IDE project file
	
	examples

		n_3_output_example.txt			example algorithm output file for n=3
	
		n_3_output_example.txt.pdf		same as above but as a pdf (to preserve alignment)
	
		n_4_output_example.txt			example algorithm output file for n=4
	
		n_4_output_example.txt.pdf		same as above but as a pdf (to preserve alignment)
	
		n_5_output_example.txt			example algorithm output file for n=5
	
		n_5_output_example.txt.pdf		same as above but as a pdf (to preserve alignment)
	
Build intructions

To build the generator, you need the FreePascal compiler (http://freepascal.org/) version 2.6 or higher or Lazarus 1.5 or older (http://www.lazarus-ide.org/). To compile with Lazarus, open the *.lpi file.

For command line compilation, run

/path/to/fpc/compiler/bin/fpc generate_catalan_monoid.pp

You must set the path to the fpc command correctly in order to reflect the installation location of the FreePascal compile on on your system. Typically, the fpc command is located in the bin folder of the compiler. 

Usage intructions

The Catalan Monoid generator executable takes one parameter for the value of n. For example, to generate all elements of the Catalan Monoid with n=4, on Unix systems use:

./generate_catalan_monoid 4


On Windows systems, run:

generate_catalan_monoid.exe 4

Feb 12, 2016

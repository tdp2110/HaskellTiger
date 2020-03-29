from subprocess import Popen, PIPE
from tempfile import TemporaryDirectory
import unittest
import os
import math


class TestCompiler(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.temp_dir = TemporaryDirectory()
    
    @classmethod
    def tearDownClass(cls):
        cls.temp_dir.cleanup()

    @property
    def compiler_outfile(self):
        return os.path.join(self.temp_dir.name, 'a.out')

    @property
    def tiger_source_file(self):
        return os.path.join(self.temp_dir.name, 'source.tiger')

    @property
    def assem_file(self):
        return os.path.join(self.temp_dir.name, 'source.s')

    def check_compiler(self, source_file, expected_output=None, run=True):
        tiger_process = Popen(['cabal', '-v0', 'new-run', 'PrintAssem',
                                source_file, '-O0', self.compiler_outfile],
                              stdout=PIPE)
            
        assem, err = tiger_process.communicate()

        tiger_process.wait()

        self.assertEqual(tiger_process.returncode, 0, (assem, err))

        with open(self.assem_file, 'wb') as f:
            f.write(assem)
        
        codegen_process = Popen(['echo', assem], stdout=PIPE)
        clang_process = Popen(['clang', self.assem_file, 'runtime/build/libtiger_rt.a',
                               '-lc++', '-o', self.compiler_outfile],
                              stdin=codegen_process.stdout)

        codegen_process.stdout.close()
        out, err = clang_process.communicate()

        codegen_process.wait()
        clang_process.wait()

        self.assertEqual(clang_process.returncode, 0, (out, err))

        if run:
            binary_process = Popen([self.compiler_outfile], stdout=PIPE)
            out, err = binary_process.communicate()

            binary_process.wait()

            #self.assertEqual(binary_process.returncode, 0, (out, err))

            self.assertEqual(out.decode('utf-8'), expected_output)
    
    def test_hello_world(self):
        self.check_compiler('examples/hello_world.tiger', 'hello world\n')

    def test_add(self):
        self.check_compiler('examples/add.tiger', str(42+1337)+'\n')

    def test_addVar(self):
        self.check_compiler('examples/addVar.tiger', str(3)+'\n')

    def test_arrays(self):
        self.check_compiler('examples/arrays.tiger', '1\n2\n3\n')

    def test_assign(self):
        self.check_compiler('examples/assign.tiger', '3\n')

    def test_call(self):
        self.check_compiler('examples/call.tiger', '43\n')

    def test_call2(self):
        self.check_compiler('examples/call2.tiger', '84')
        
    def test_call5(self):
        self.check_compiler('examples/call5.tiger', '3\n')
        
    def test_call6(self):
        self.check_compiler('examples/call6.tiger', '1339\nhello world\n')

    def test_closure(self):
        self.check_compiler('examples/closure.tiger', '1379\n')
        
    def test_fact(self):
        self.check_compiler('examples/fact.tiger', str(math.factorial(10))+'\n')
        
    def test_pair(self):
        self.check_compiler('examples/pair.tiger', '3\n')
        
    def test_pair1(self):
        self.check_compiler('examples/pair1.tiger', '1\n')
        
    def test_printVar(self):
        self.check_compiler('examples/printVar.tiger', chr(8))
        
    def test_strings(self):
        self.check_compiler('examples/strings.tiger', 'hello world!\n12\n')
        
    def test_sumList(self):
        self.check_compiler('examples/sumList.tiger', '45\n')
        
    def test_sumList2(self):
        self.check_compiler('examples/sumList2.tiger', '3\n')
        
    def test_eight_queens(self):
        with open('examples/eight_queens_output.txt', 'r') as f:
            eight_queens_output = f.read()
        
        self.check_compiler('examples/eight_queens.tiger', eight_queens_output)

    def test_merge(self):
        self.check_compiler('examples/merge.tiger', run=False)
        
if __name__ == '__main__':
    unittest.main()

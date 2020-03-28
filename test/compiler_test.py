from subprocess import Popen, PIPE
from tempfile import TemporaryDirectory
import unittest
import os


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

    def check_compiles(self, source_file, expected_output):
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
        
        binary_process = Popen([self.compiler_outfile], stdout=PIPE)
        out, err = binary_process.communicate()

        binary_process.wait()

        #self.assertEqual(binary_process.returncode, 0, (out, err))
        
        self.assertEqual(out.decode('utf-8'), expected_output)
    
    def test_hello_world(self):
        self.check_compiles('examples/hello_world.tiger', 'hello world\n')

if __name__ == '__main__':
    unittest.main()

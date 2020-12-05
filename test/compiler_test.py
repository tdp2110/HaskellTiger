from subprocess import Popen, PIPE
from tempfile import TemporaryDirectory
import unittest
import os
import math


class TestNativeCompiler(unittest.TestCase):
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
    def assem_file(self):
        return os.path.join(self.temp_dir.name, 'out.s')

    def check_compiler(self, source_file, expected_output=None, run=True, expected_retcode=0):
        base_compile_command = ['cabal', '-v0', 'new-run', 'tigerc', '--',
                                source_file]

        def check_at_optim_level(compile_command):        
            tiger_process = Popen(compile_command,
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

                self.assertEqual(binary_process.returncode, expected_retcode, (out, err))
                self.assertEqual(out.decode('utf-8'), expected_output)

            return assem

        check_at_optim_level(base_compile_command + ['--O0'])
        return check_at_optim_level(base_compile_command)

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
        
    def test_fib(self):
        self.check_compiler('examples/fibonacci.tiger', '75025\n')
        
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

    def test_local_access_1(self):
        self.check_compiler('examples/local-access-1.tiger', '1337\n')

    def test_local_access_2(self):
        self.check_compiler('examples/local-access-2.tiger', '1337\n')

    def test_local_access_3(self):
        self.check_compiler('examples/local-access-3.tiger', '42\n')

    def test_local_access_4(self):
        self.check_compiler('examples/local-access-4.tiger', '42\n')

    def test_mutual_recursion(self):
        self.check_compiler('examples/mutualRecursion.tiger', '0\n1\n1\n0\n')
        
    def test_redzone(self):
        assem = self.check_compiler('examples/redzone.tiger', '190\n')
        # if would be better to do this in a more structured way :D
        assem = assem.decode('utf-8')
        self.assertIn('_main', assem)
        _, assem1, *_ = assem.split('_main')
        self.assertIn('## (f,', assem1)
        self.assertNotIn('sub rsp', assem1)

    def test_dead_stores(self):
        assem = self.check_compiler('examples/dead-stores.tiger', '42\n')
        assem = assem.decode('utf-8')

    def test_unused_functions(self):
        assem = self.check_compiler('examples/unused-funs.tiger', 'hello world\n')
        assem = assem.decode('utf-8')
        self.assertNotIn('fact', assem)
        
    def test_loop_register_allocator(self):
        self.check_compiler('examples/loopRegAllocator.tiger', run=False)

    def test_divs(self):
        self.check_compiler('examples/divs.tiger', '2\n1\n')
        
    def test_call_noreturn(self):
        assem = self.check_compiler('examples/constexpr-div.tiger', run=False, expected_retcode=1)
        assem = assem.decode('utf-8')
        self.assertEqual(assem.strip().splitlines()[-1].strip().split(), ['call', 'rax'])

class TestLLVMCodegen(unittest.TestCase):
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
    def assem_file(self):
        return os.path.join(self.temp_dir.name, 'out.ll')

    def check_compiler(self, source_file, expected_output=None, run=True, expected_retcode=0):
        compile_command = ['cabal', '-v0', 'new-run', 'tigerc', '--', '--emit-llvm',
                           source_file]

        tiger_process = Popen(compile_command,
                              stdout=PIPE)

        assem, err = tiger_process.communicate()

        tiger_process.wait()

        self.assertEqual(tiger_process.returncode, 0, (assem, err))

        with open(self.assem_file, 'wb') as f:
            f.write(assem)

        codegen_process = Popen(['echo', assem], stdout=PIPE)
        clang_process = Popen(['clang', self.assem_file, 'runtime/build/libtiger_rt.a',
                               '-lc++', '-o', self.compiler_outfile, '-Wno-override-module'],
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

            self.assertEqual(binary_process.returncode, expected_retcode, (out, err))
            self.assertEqual(out.decode('utf-8'), expected_output)

        return assem

    def test_hello_world(self):
        self.check_compiler('examples/assign2.tiger', '0\n1\n')

    def test_fib(self):
        self.check_compiler('examples/fibonacci.tiger', '75025\n')
        
    def test_loop(self):
        self.check_compiler('examples/loop.tiger', '4950\n')
        
    def test_call(self):
        self.check_compiler('examples/call.tiger', '43\n')

    def test_call2(self):
        self.check_compiler('examples/call2.tiger', '84')
        
    def test_call_7(self):
        self.check_compiler('examples/call7.tiger', '3\n')
        
    def test_hello_world(self):
        self.check_compiler('examples/hello_world.tiger', 'hello world\n')

    def test_hello_world_2(self):
        self.check_compiler('examples/strings.tiger', 'hello world!\n12\n')
        
if __name__ == '__main__':
    unittest.main()

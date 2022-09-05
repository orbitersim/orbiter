using System;
using System.Collections.Generic;
using System.Linq;
using System.IO;
using System.Diagnostics;

namespace InterfaceBuilder
{
    class Program
    {
        struct MethodData {
            public string whs;
            public string par;
            public string ret;
            public string fnc;
        }

        static string ParseLine(string a)
        {
            char[] wb = { ' ', '\t' }; // Remove these...
            char[] dl = { ',', ')', ' ', '\t', '(' }; // from front of these
            char[] tm = { '*', '&' }; // Things to move

            string c = "";
            a += ' ';
            for (int i = 0; i < a.Length - 1; i++)
            {
                char ch = (a[i] == '\t') ? ' ' : a[i]; // Remove tabs
                if (wb.Contains(ch) && dl.Contains(a[i + 1])) continue;
                c += ch;
            }

            char[] s = new char[256];
            
            s = c.ToCharArray();
        
            for (int i = 0; i < s.Length; i++)
            {
                if (s[i] == 0) break;
                if (s[i] == ' ' && tm.Contains(s[i + 1]))
                {
                    s[i] = s[i + 1];
                    s[i + 1] = ' ';
                }
            }

            c = "";
            for (int i = 0; i < s.Length; i++)
            {
                if (i > 0 && s[i] == ' ' && s[i - 1] == ' ') continue;
                c += s[i];
            }
            return c;
        }


        static void Main(string[] args)
        {
            bool bInterface = false;
            bool bParse = false;
            char[] sep = { ' ' };
            int Brackets = 0;

            string wd = Directory.GetCurrentDirectory();

            if (args.Length != 3)
            {
                Debug.WriteLine("Invalid arguments");              
                return;
            }
          
            Debug.WriteLine("HdrInput = {0}", args[0]);
            Debug.WriteLine("HdrOutput = {0}", args[1]);
            Debug.WriteLine("CppFile = {0}", args[2]);
            Debug.WriteLine("WorkingDir = {0}", Directory.GetCurrentDirectory());
          

            if (!File.Exists(args[0]) || !File.Exists(args[2]))
            {
                Debug.WriteLine("InterfaceBuilder ERROR: Input file doesn't exists. Press any key to continue");              
                return;
            }


            StreamReader In = new StreamReader(args[0]);
            StreamWriter Hdr = new StreamWriter(args[1]);
            StreamReader InCpp = new StreamReader(args[2]);

            Queue<string> CppFile = new Queue<string>();

            while (true)
            {
                string line = InCpp.ReadLine();
                if (line == null) break;
                CppFile.Enqueue(line);
            }

            InCpp.Close();
            InCpp.Dispose();

            List<MethodData> Methods = new List<MethodData>();


            // Take a first look at the source file gcCore.h and create function database =======
            //
            while (true)
            {
                string line = In.ReadLine();

                if (line == null) break;

                if (line.Contains("INTERFACE_BUILDER"))
                {
                    bInterface = true;
                }

                if (bInterface)
                {
                    Brackets += line.Count(f => f == '{');
                    Brackets -= line.Count(f => f == '}');
                    if (Brackets > 0) bParse = true;
                }

                if (bParse && Brackets == 0)
                {
                    bParse = false;
                    bInterface = false;
                }

                if (bParse)
                {
                    if (line.Contains("gc_interface"))
                    {
                        MethodData md;
                        md.whs = "";
                        md.ret = "error";
                        md.fnc = "error";
                        md.par = "";

                        // Get White space information
                        foreach (var x in line)
                        {
                            if (x == ' ') md.whs += x;
                            else if (x == '\t') md.whs += x;
                            else break;
                        }
                
                        string line2 = line.Replace("gc_interface", "");

                        bool bSkip = false;
                        string temp = "";

                        // Remove default params
                        foreach (var c in line2)
                        {
                            if (c == ';') break;
                            if (c == '=') bSkip = true;
                            if (bSkip && (c == ',' || c == ')')) bSkip = false;
                            if (!bSkip) temp += c;
                        }

                        temp = ParseLine(temp.Trim()).Trim();
              
                        int p = temp.IndexOf('(');
                        md.par = temp.Substring(p); //Get Parameters
                        string fnc = temp.Remove(p); // Remove parameters section from "temp"            
                        string[] cmd = fnc.Split(sep);

                        if (cmd.Length == 2)
                        {
                            md.ret = cmd[0]; md.fnc = cmd[1];
                        }
                        
                        Methods.Add(md);
                    }
                }     
            }



            // Create End User Header File gcCoreAPI.h ==================================================
            //
            // Back to begining
            In.BaseStream.Seek(0, SeekOrigin.Begin);
            Hdr.WriteLine("");
            Hdr.WriteLine("// WARNING ===============================================================================");
            Hdr.WriteLine("// This is computer generated file. Do not modify. Make modifications to gcCore.h instead.");
            Hdr.WriteLine("// WARNING ===============================================================================");
            Hdr.WriteLine("");

            while (true)
            {
                string line = In.ReadLine();

                if (line == null)
                {
                    In.Close();
                    Hdr.Close();
                    In.Dispose();
                    Hdr.Dispose();
                    break;
                }

                if (line.Contains("INTERFACE_BUILDER"))
                {
                    bInterface = true;
                }
           

                // Write function pointers to End User Header.
                //
                if (line.Contains("#define") && line.Contains("fnc_typedefs"))
                {
                    foreach (var x in Methods)
                    {
                        Hdr.WriteLine("\t" + x.ret + " (__cdecl * p" + x.fnc + ")" + x.par + ";");
                    }
                    continue;
                }


                // Write function binders to End User Header.
                //
                if (line.Contains("#define") && line.Contains("fnc_binder"))
                {
                    foreach (var x in Methods)
                    {
                        Hdr.WriteLine("\t\tpBindCoreMethod((void**)&p" + x.fnc + ", \"" + x.fnc + "\");");
                    }
                    continue;
                }
               

                if (bInterface)
                {
                    Brackets += line.Count(f => f == '{');
                    Brackets -= line.Count(f => f == '}');
                    if (Brackets > 0) bParse = true;
                }

                if (bParse && Brackets == 0)
                {
                    bParse = false;
                    bInterface = false;
                }


                if (bParse)
                {
                    if (line.Contains("gc_interface"))
                    {
                        string tmp = line.Replace("gc_interface ", "");
                        line = tmp.Replace(";", "");
                        Hdr.WriteLine(line);
              
                        foreach(var x in Methods)
                        {
                            if (line.Contains(x.fnc))
                            {
                                Hdr.WriteLine(x.whs + "{");
                                char[] delim = { ',' };
                                string q = x.par;

                                string fnc = x.whs + "\t" + "return p" + x.fnc;

                                if (x.par != "()" && x.par != "(void)" && x.par != "( )")
                                {
                                    fnc += '(';

                                    // Remove prackets
                                    q.Remove(q.IndexOf('('), 1);

                                    string[] list = q.Split(delim);

                                    foreach (var pair in list)
                                    {
                                        string[] par = pair.Trim().Split(sep);
                                        string var = par[par.Length - 1];
                                        fnc += var;
                                        if (pair != list.Last()) fnc += ", ";
                                    }
                                }
                                else fnc += x.par;

                                Hdr.WriteLine(fnc + ";");
                                Hdr.WriteLine(x.whs + "}");
                                break;
                            }
                        }
                    }
                    else
                    {
                        Hdr.WriteLine(line);
                    }
                }
                else
                {
                    Hdr.WriteLine(line);
                }
            }


            // Modify gcCore.cpp File ==================================================
            //
            StreamWriter Cpp = new StreamWriter(args[2]);

            while (CppFile.Count > 0)
            {
                string line = CppFile.Dequeue();

                // Write function binders to End User Header.
                //
                if (line.Contains("#define") && line.Contains("binder_start"))
                {
                    Cpp.WriteLine(line);
       
                    foreach (var x in Methods)
                    {
                        Cpp.WriteLine("\tif (strcmp(name,\"" + x.fnc + "\")==0) *ppFnc = &gcCore2::" + x.fnc + ";");
                    }

                    while (CppFile.Count > 0)
                    {
                        string line2 = CppFile.Dequeue();
                        if (line2.Contains("#define") && line2.Contains("binder_end"))
                        {
                            Cpp.WriteLine(line2);
                            break;
                        }
                    }
                }
                else
                {
                    Cpp.WriteLine(line);
                }
            }

            Cpp.Close();
            Cpp.Dispose();
        }
    }
}

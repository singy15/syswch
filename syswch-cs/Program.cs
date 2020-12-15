using System;
using System.IO;
using System.Net.Http;
using System.Collections.Generic;
using System.Threading.Tasks;
using System.Text;

namespace syswch
{
    class Program
    {
        private static string port;
        private static System.Net.Sockets.TcpClient tcp;
        private static System.Net.Sockets.NetworkStream ns;

        static void Main(string[] args)
        {
            port = args[1];

            FileSystemWatcher watcher = new FileSystemWatcher();
            watcher.Path = args[0];
            watcher.Filters.Add("*.lisp");
            watcher.Filters.Add("*.asd");
            watcher.NotifyFilter = 
                NotifyFilters.FileName
                | NotifyFilters.DirectoryName
                | NotifyFilters.LastWrite
                | NotifyFilters.LastAccess;
            watcher.IncludeSubdirectories = true;
            //watcher.InternalBufferSize = 4096


            // Set event handlers 
            watcher.Created += new FileSystemEventHandler(Changed);
            watcher.Changed += new FileSystemEventHandler(Changed);
            watcher.Deleted += new FileSystemEventHandler(Changed);


            // Connect server
            tcp = new System.Net.Sockets.TcpClient("127.0.0.1", int.Parse(port));

            ns = tcp.GetStream();
            ns.ReadTimeout = 10000;
            ns.WriteTimeout = 10000;


            // Watch filesystem
            watcher.EnableRaisingEvents = true;
            while(true)
            {
                System.Threading.Thread.Sleep(500);
            }
        }

        public static void Changed(object sender, FileSystemEventArgs e) 
        {
            if(Path.GetFileName(e.FullPath) == "i.lisp")
            {
                return;
            }

            Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);
            System.Text.Encoding enc = System.Text.Encoding.GetEncoding("UTF-8");
            byte[] sendBytes = enc.GetBytes(System.IO.Path.GetFullPath(e.FullPath) /*+ '\r'*/ + '\n');
            ns.Write(sendBytes, 0, sendBytes.Length);
        }
    }
}


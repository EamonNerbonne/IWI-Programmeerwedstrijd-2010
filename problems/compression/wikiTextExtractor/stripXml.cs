using System;
using System.Text;
using System.Text.RegularExpressions;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Globalization;
using System.Collections;
namespace StripXml {
	static class Program {
		public static int Main(string[] args) {
			if (args.Length != 2) {
				Console.WriteLine("Usage: <wiki file dir> <output_file>");
				return 1;
			} else {
				using (var fs = File.OpenWrite(args[1]))
				using (var writer = new StreamWriter(fs)) {
					Console.WriteLine("Processing files");
					foreach (var line in CleanLines(LinesFromAllFiles(args[0])))
						writer.WriteLine(line);
					return 0;
				}
			}
		}

		static IEnumerable<string> LinesFromAllFiles(string dir) { return FindWikiFiles(dir).SelectMany(File.ReadAllLines); }

		static IEnumerable<string> FindWikiFiles(string dir) { return Directory.GetFiles(dir, "wiki*", SearchOption.AllDirectories); }


		static IEnumerable<string> CleanLines(IEnumerable<string> lines) {
			StringBuilder sb = new StringBuilder();

			foreach (var line in lines) {
				bool closingLine = line.Contains("</doc>");
				string noxml = tagRegex.Replace(line, "");
				bool lastCharUnknownOrSpace = true;
				string decomposed = noxml.Normalize(NormalizationForm.FormKD);// to strip weird accents

				foreach (char c in decomposed) {

					if (c == '.' || c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c >= '0' && c <= '9') {
						lastCharUnknownOrSpace = false;
						sb.Append(c);
					} else if (!lastCharUnknownOrSpace && Canonicalize.IsReasonable(c)) {
						sb.Append(' ');
						lastCharUnknownOrSpace = true;
					}
				}

				if (closingLine) {
					if (lastCharUnknownOrSpace && sb.Length > 0)
						sb.Length = sb.Length - 1;
					yield return sb.ToString();
					sb.Clear();
				} else {
					if (!lastCharUnknownOrSpace)
						sb.Append(' ');
				}
			}
			if (sb.Length > 1) {
				sb.Length = sb.Length - 1;
				yield return sb.ToString();
			}
		}


		static Regex tagRegex = new Regex(@"</?(doc|a)[^>]*>", RegexOptions.Compiled);
		//the input isn't valid xml; this regex works.
	}
	public static class Canonicalize {
		private static BitArray reasonablechar;
		static Canonicalize() {
			reasonablechar = new BitArray((int)char.MaxValue + 1);
			//translatedchar = new char[(int)char.MaxValue + 1];
			foreach (int c in Enumerable.Range(0, (int)char.MaxValue + 1)) {
				UnicodeCategory cat = CharUnicodeInfo.GetUnicodeCategory((char)c);
				reasonablechar[c] = c == '\n' || c == '\t' || (
					cat != UnicodeCategory.Format &&
					cat != UnicodeCategory.Control &&
					cat != UnicodeCategory.NonSpacingMark &&
					cat != UnicodeCategory.OtherNotAssigned &&
					cat != UnicodeCategory.PrivateUse);
			}
		}

		public static bool IsReasonable(char c) { return reasonablechar[(int)c]; }
	}
}

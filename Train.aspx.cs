using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.IO;

public partial class Admin_Train : System.Web.UI.Page
{
    save s = new save();
    protected void Page_Load(object sender, EventArgs e)
    {

    }
    protected void Button1_Click(object sender, EventArgs e)
    {
        if (DropDownList1.Text == "Godaddy")
        {
            string getvalue = Get_HTML("http://www.mouthshut.com/websites/Godaddy-com-reviews-925641288");
            // Response.Write(getvalue);
            using (StreamWriter outfile = new StreamWriter(Server.MapPath("~/Textfiles/TextFile.txt")))
            {
                outfile.Write(getvalue);
            }

        }
        if (DropDownList1.Text == "Bigrock")
        {
            string getvalue = Get_HTML("http://www.mouthshut.com/websites/Bigrock-in-reviews-925641276");
            // Response.Write(getvalue);
            using (StreamWriter outfile = new StreamWriter(Server.MapPath("~/Textfiles/TextFile7.txt")))
            {
                outfile.Write(getvalue);
            }

        }
        if (DropDownList1.Text == "Bluehost")
        {
            string getvalue = Get_HTML("http://www.mouthshut.com/websites/Bluehost-in-reviews-925872109");
            // Response.Write(getvalue);
            using (StreamWriter outfile = new StreamWriter(Server.MapPath("~/Textfiles/TextFile8.txt")))
            {
                outfile.Write(getvalue);
            }

        }


        
        
    }
    public static bool SearchWord(string word, string path)
    {
        StreamReader re = File.OpenText(path);
        string input = null;
        while ((input = re.ReadLine()) != null)
        {
            if (input.IndexOf(word, 0) > 0)
            {

                return true;
            }
        }

        return false;
    }
   
    public static string Get_HTML(string Url)
    {
        System.Net.WebResponse Result = null;
        string Page_Source_Code;
        try
        {
            System.Net.WebRequest req = System.Net.WebRequest.Create(Url);
            Result = req.GetResponse();
            System.IO.Stream RStream = Result.GetResponseStream();
            System.IO.StreamReader sr = new System.IO.StreamReader(RStream);
            new System.IO.StreamReader(RStream);
            Page_Source_Code = sr.ReadToEnd();
            sr.Dispose();
        }
        catch
        {
            // error while reading the url: the url dosen’t exist, connection problem...
            Page_Source_Code = "";
        }
        finally
        {
            if (Result != null) Result.Close();
        }
        return Page_Source_Code;
    }
    protected void ddlrest_SelectedIndexChanged(object sender, EventArgs e)
    {

    }
    protected void Button2_Click(object sender, EventArgs e)
    {
        if (DropDownList1.Text == "Godaddy")
        {
            StreamReader sr = new StreamReader(Server.MapPath("~/Textfiles/TextFile.txt"));
            String line = sr.ReadToEnd();
            sr.Close();
            // textBox2.Text = line;
            string positiveKeyword = "good,excellent,delight,verygood,awesome";
            string[] words = positiveKeyword.Split(',');
            string negativeKeyword = "bad,worst,awful,extremely bad";
            string[] negativewords = negativeKeyword.Split(',');
            int i = 0, j = 0;
            foreach (string str in words)
            {
                foreach (string nstr in negativewords)
                {
                    if (SearchWord(str, Server.MapPath("~/Textfiles/TextFile.txt")) == true)
                    {
                        i++;
                    }
                    else if (SearchWord(nstr, Server.MapPath("~/Textfiles/TextFile.txt")) == true)
                    {
                        j++;
                    }
                }
            }

            int rate = i - j;
            int totalrate = i + j;
            decimal entropy = rate / totalrate;
            decimal log =Convert.ToDecimal( Math.Log(totalrate, 2));
            s.insert("update [Rating] set PositiveRating='" + i + "',NegativeRating='" + j + "',TotalRating='" + totalrate + "',Entropy='" + entropy + "',Ratinglog='"+log+"' where [CompanyName]='" + DropDownList1.Text + "'");

        }
        if (DropDownList1.Text == "Bigrock")
        {
            StreamReader sr = new StreamReader(Server.MapPath("~/Textfiles/TextFile7.txt"));
            String line = sr.ReadToEnd();
            sr.Close();
            // textBox2.Text = line;
            string positiveKeyword = "good,excellent,delight,verygood,awesome";
            string[] words = positiveKeyword.Split(',');
            string negativeKeyword = "bad,worst,awful,extremely bad";
            string[] negativewords = negativeKeyword.Split(',');
            int i = 0, j = 0;
            foreach (string str in words)
            {
                foreach (string nstr in negativewords)
                {
                    if (SearchWord(str, Server.MapPath("~/Textfiles/TextFile7.txt")) == true)
                    {
                        i++;
                    }
                    else if (SearchWord(nstr, Server.MapPath("~/Textfiles/TextFile7.txt")) == true)
                    {
                        j++;
                    }
                }
            }

            int rate = i - j;
            int totalrate = i + j;
            decimal entropy = Convert.ToDecimal(rate / totalrate);
            decimal log = Convert.ToDecimal(Math.Log(totalrate, 2));
            s.insert("update [Rating] set PositiveRating='" + i + "',NegativeRating='" + j + "',TotalRating='" + totalrate + "',Entropy='" + entropy + "',Ratinglog='" + log + "' where [CompanyName]='" + DropDownList1.Text + "'");

        }
        if (DropDownList1.Text == "Bluehost")
        {
            StreamReader sr = new StreamReader(Server.MapPath("~/Textfiles/TextFile8.txt"));
            String line = sr.ReadToEnd();
            sr.Close();
            // textBox2.Text = line;
            string positiveKeyword = "good,excellent,delight,verygood,awesome";
            string[] words = positiveKeyword.Split(',');
            string negativeKeyword = "bad,worst,awful,extremely bad";
            string[] negativewords = negativeKeyword.Split(',');
            int i = 0, j = 0;
            foreach (string str in words)
            {
                foreach (string nstr in negativewords)
                {
                    if (SearchWord(str, Server.MapPath("~/Textfiles/TextFile8.txt")) == true)
                    {
                        i++;
                    }
                    else if (SearchWord(nstr, Server.MapPath("~/Textfiles/TextFile8.txt")) == true)
                    {
                        j++;
                    }
                }
            }

            int rate = i - j;
            int totalrate = i + j;
            decimal entropy = rate / totalrate;
            decimal log = Convert.ToDecimal(Math.Log(totalrate, 2));
            s.insert("update [Rating] set PositiveRating='" + i + "',NegativeRating='" + j + "',TotalRating='" + totalrate + "',Entropy='" + entropy + "',Ratinglog='" + log + "' where [CompanyName]='" + DropDownList1.Text + "'");
        }
    }
}
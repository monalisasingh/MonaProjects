using System;
using System.Data.SqlClient;
using System.Configuration;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.UI;
using System.Web.UI.WebControls;
/// <summary>
/// Summary description for save
/// </summary>
public class save
{
	public save()
	{
        
		//
		// TODO: Add constructor logic here
		//
	}
    public SqlConnection con = new SqlConnection(ConfigurationManager.ConnectionStrings["ServerProvider"].ConnectionString);
    public void insert(string query)
    {
        con.Open();
        SqlCommand cmd = new SqlCommand(query, con);
        cmd.ExecuteNonQuery();
        con.Close();
          }
    //adminlogin
    public void adminlogin(string username, string password, Label Label5)
    {
        con.Open();
        SqlCommand cmd = new SqlCommand("select username,password from Adminlogin where username='" + username + "' and password='" + password + "'", con);
        SqlDataReader dr = cmd.ExecuteReader();
        if (dr.Read())
        {
            if (username == dr[0].ToString() || password == dr[1].ToString())
            {
                HttpContext.Current.Session["admin"] = username;
                HttpContext.Current.Response.Redirect("~/admin/Adminhome.aspx");
            }
        }
        else
        {
            Label5.Visible = true;
            Label5.ForeColor = System.Drawing.Color.Black;
            Label5.Text = "Invalid Username or password";


        }
    }

    //managerlogin
    public void Managerlogin(string username, string password, Label Label5)
    {
        con.Open();
        SqlCommand cmd = new SqlCommand("select email,password from ManagerRegistration where email='" + username + "' and password='" + password + "'", con);
        SqlDataReader dr = cmd.ExecuteReader();
        if (dr.Read())
        {
            if (username == dr[0].ToString() || password == dr[1].ToString())
            {
                HttpContext.Current.Session["manager"] = username;
                HttpContext.Current.Response.Redirect("~/manager/Managerhome.aspx");
            }
        }
        else
        {
            Label5.Visible = true;
            Label5.ForeColor = System.Drawing.Color.Black;
            Label5.Text = "Invalid Username or password";


        }
    }

    
    
    //user login
    public void login(string username, string password, Label Label5)
    {
        con.Open();
        SqlCommand cmd = new SqlCommand("select email,password from UserRegistration where email='" + username + "' and password='" + password + "'", con);
        SqlDataReader dr = cmd.ExecuteReader();
        if (dr.Read())
        {
            if (username == dr[0].ToString() || password == dr[1].ToString())
            {
                HttpContext.Current.Session["user"] = username;
                HttpContext.Current.Response.Redirect("~/user/cab.aspx");
            }
        }
        else
        {
            Label5.Visible = true;
            Label5.ForeColor = System.Drawing.Color.Black;
            Label5.Text = "Invalid Username or password";


        }
    }

}


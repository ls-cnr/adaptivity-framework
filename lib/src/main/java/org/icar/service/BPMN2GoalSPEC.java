package org.icar.service;

/**
 * Servlet implementation class BPMN2GoalSPEC
 */

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;

import org.json.JSONObject;

import org.icar.bpmn2goal.bpmn_parser;


@WebServlet("/GoalsFromBPMN")
public class BPMN2GoalSPEC extends HttpServlet {
    private static final long serialVersionUID = 1L;

    public BPMN2GoalSPEC() {
        super();
    }

    @Override
    protected void doGet(HttpServletRequest req, HttpServletResponse res) throws ServletException, IOException {
        res.setContentType("text/html; charset=utf-8");
        try (PrintWriter out = res.getWriter()) {
            out.println("<html><body>");
            out.println("Servlet working<br>");
            out.println(getServletContext().getServerInfo());
            out.println("</body></html>");
        }
    }

    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        String data = request.getParameter("bpmnDiagramm");
        InputStream is = new ByteArrayInputStream(data.getBytes());
        bpmn_parser parser = new bpmn_parser(is);
        String goalString = parser.fullFromInputStream();

        String callback = request.getParameter("callback");
        PrintWriter out = response.getWriter();
        response.setContentType("text/html");
        response.setHeader("Cache-control", "no-cache, no-store");
        response.addHeader("Access-Control-Allow-Origin", "*");
        response.setHeader("Pragma", "no-cache");
        response.setHeader("Expires", "-1");

        JSONObject obj = new JSONObject();
        obj.put("goals", goalString);
        if (callback != null) {
            out.println(callback + "(" + obj.toString() + ");");
        } else {
            out.println(obj.toString());
        }

        out.close();
    }

}

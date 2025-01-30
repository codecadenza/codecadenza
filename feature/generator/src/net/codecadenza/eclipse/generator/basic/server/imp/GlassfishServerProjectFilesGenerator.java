/*
 * This file is part of CodeCadenza, a set of tools, libraries and plug-ins
 * for modeling and creating Java-based enterprise applications.
 * For more information visit:
 *
 * https://github.com/codecadenza/
 *
 * This software is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
 */
package net.codecadenza.eclipse.generator.basic.server.imp;

import static net.codecadenza.eclipse.shared.Constants.UTF_8;

import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Generator for basic source and configuration files for applications that are deployed on Glassfish
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class GlassfishServerProjectFilesGenerator extends AbstractServerProjectFilesGenerator {
	public static final String GLASSFISH_WEBXML = "glassfish-web.xml";

	/**
	 * Constructor
	 * @param project
	 */
	public GlassfishServerProjectFilesGenerator(Project project) {
		this.project = project;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.basic.server.imp.AbstractServerProjectFilesGenerator#createDataSource()
	 */
	@Override
	public String createDataSource() {
		final var b = new StringBuilder();
		final String dsName = project.getDataSource().getName();
		final String connectionURL = project.getDataSource().getConnectionURL();
		final String driverName = project.getDataSource().getDriverName();
		final var poolName = dsName + "Pool";
		final String userName = project.getDataSource().getUserName();
		final String password = project.getDataSource().getPassword();

		b.append("<?xml version=\"1.0\" encoding=\"" + UTF_8 + "\"?>\n");
		b.append("<!DOCTYPE resources PUBLIC \"-//GlassFish.org//DTD GlassFish Application Server 3.1 Resource Definitions//EN\" ");
		b.append("\"http://glassfish.org/dtds/glassfish-resources_1_5.dtd\">\n");
		b.append("<resources>\n\n");
		b.append("\t<jdbc-connection-pool name=\"java:app/jdbc/" + poolName + "\" res-type=\"java.sql.Driver\" ");
		b.append("driver-classname=\"" + driverName + "\" ");
		b.append("pool-resize-quantity=\"1\" max-pool-size=\"5\" steady-pool-size=\"1\" ");
		b.append("transaction-isolation-level=\"read-committed\" ");
		b.append("statement-timeout-in-seconds=\"60\">\n");
		b.append("\t\t<property name=\"url\" value=\"" + connectionURL + "\"/>\n");
		b.append("\t\t<property name=\"user\" value=\"" + userName + "\"/>\n");
		b.append("\t\t<property name=\"password\" value=\"" + password + "\"/>\n");
		b.append("\t\t<property name=\"useSSL\" value=\"false\"/>\n");
		b.append("\t\t<property name=\"driverType\" value=\"4\"/>\n");
		b.append("\t</jdbc-connection-pool>\n\n");
		b.append("\t<jdbc-resource jndi-name=\"" + project.getDataSourceJNDIName());
		b.append("\" pool-name=\"java:app/jdbc/" + poolName + "\"/>\n\n");
		b.append("</resources>\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.basic.server.imp.AbstractServerProjectFilesGenerator#getVendorWebXMLName()
	 */
	@Override
	public String getVendorWebXMLName() {
		return GLASSFISH_WEBXML;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.basic.server.imp.AbstractServerProjectFilesGenerator#createVendorWebXML()
	 */
	@Override
	public String createVendorWebXML() {
		final var b = new StringBuilder();
		b.append("<?xml version=\"1.0\" encoding=\"" + UTF_8 + "\"?>\n");
		b.append("<!DOCTYPE glassfish-web-app PUBLIC \"-//GlassFish.org//DTD GlassFish Application Server 3.1 Servlet 3.0//EN\" ");
		b.append("\"http://glassfish.org/dtds/glassfish-web-app_3_0-1.dtd\">\n");
		b.append("<glassfish-web-app>\n\n");
		b.append("\t<context-root>/" + project.getCode() + "</context-root>\n");

		// Add this configuration to enable logging (see https://github.com/payara/Payara/issues/5898)
		b.append("\t<class-loader delegate=\"false\"/>\n");

		final DTOBean logOnDTO = project.getApplicationLogOnDTO();
		final BoundaryBean logOnBoundary = project.getLogOnBoundary();

		if (logOnDTO != null && logOnBoundary != null)
			project.getRoles().forEach(role -> {
				b.append("\n\t<security-role-mapping>\n");
				b.append("\t\t<role-name>" + role.getName() + "</role-name>\n");
				b.append("\t\t<group-name>" + role.getName() + "</group-name>\n");
				b.append("\t</security-role-mapping>\n");
			});

		b.append("\n</glassfish-web-app>\n");

		return b.toString();
	}

}

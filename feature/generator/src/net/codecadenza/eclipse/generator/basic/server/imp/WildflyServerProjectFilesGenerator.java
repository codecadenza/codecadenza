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
 * Generator for basic source and configuration files for applications that are deployed on Wildfly
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class WildflyServerProjectFilesGenerator extends AbstractServerProjectFilesGenerator {
	public static final String JBOSS_WEBXML = "jboss-web.xml";

	/**
	 * Constructor
	 * @param project
	 */
	public WildflyServerProjectFilesGenerator(Project project) {
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
		final var poolName = dsName + "Pool";
		final String userName = project.getDataSource().getUserName();
		final String password = project.getDataSource().getPassword();

		b.append("<?xml version=\"1.0\" encoding=\"" + UTF_8 + "\"?>\n");
		b.append("<datasources xmlns=\"urn:jboss:domain:datasources:7.2\">\n\n");
		b.append("\t<datasource jndi-name=\"" + project.getDataSourceJNDIName());
		b.append("\" enabled=\"true\" use-java-context=\"true\" pool-name=\"");
		b.append(poolName + "\">\n");
		b.append("\t\t<connection-url>" + connectionURL + "</connection-url>\n");
		b.append("\t\t<driver>" + project.getDatabase().getVendorGroup().getName().toLowerCase() + ".jar</driver>\n");
		b.append("\t\t<pool>\n");
		b.append("\t\t\t<max-pool-size>30</max-pool-size>\n");
		b.append("\t\t</pool>\n");
		b.append("\t\t<security>\n");
		b.append("\t\t\t<user-name>" + userName + "</user-name>\n");
		b.append("\t\t\t<password>" + password + "</password>\n");
		b.append("\t\t</security>\n");
		b.append("\t</datasource>\n\n");
		b.append("</datasources>\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.basic.server.imp.AbstractServerProjectFilesGenerator#getVendorWebXMLName()
	 */
	@Override
	public String getVendorWebXMLName() {
		return JBOSS_WEBXML;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.basic.server.imp.AbstractServerProjectFilesGenerator#createVendorWebXML()
	 */
	@Override
	public String createVendorWebXML() {
		final var b = new StringBuilder();
		final DTOBean logOnDTO = project.getApplicationLogOnDTO();
		final BoundaryBean logOnBoundary = project.getLogOnBoundary();

		b.append("<?xml version=\"1.0\" encoding=\"" + UTF_8 + "\"?>\n");
		b.append("<jboss-web xmlns:xsi=\"http://www.w3.org/2001/XMLSchema\" ");
		b.append("xsi:schemaLocation=\"http://www.jboss.org/schema/jbossas/jboss-web_15_0.xsd\" ");
		b.append("version=\"15.0\">\n\n");
		b.append("\t<context-root>/" + project.getCode() + "</context-root>\n");

		if (logOnDTO != null && logOnBoundary != null)
			b.append("\t<security-domain>jaspi-default</security-domain>\n");

		b.append("\n</jboss-web>\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.basic.server.imp.AbstractServerProjectFilesGenerator#createDeploymentStructureXML()
	 */
	@Override
	public String createDeploymentStructureXML() {
		final var b = new StringBuilder();
		b.append("<?xml version=\"1.0\" encoding=\"" + UTF_8 + "\"?>\n");
		b.append("<jboss-deployment-structure xmlns=\"urn:jboss:deployment-structure:1.2\">\n\n");
		b.append("\t<deployment>\n");
		b.append("\t\t<exclude-subsystems>\n");
		b.append("\t\t\t<subsystem name=\"logging\"/>\n");
		b.append("\t\t</exclude-subsystems>\n");
		b.append("\t\t<exclusions>\n");
		b.append("\t\t\t<module name=\"org.dom4j\"/>\n");
		b.append("\t\t</exclusions>\n");
		b.append("\t</deployment>\n\n");
		b.append("</jboss-deployment-structure>\n");

		return b.toString();
	}

}

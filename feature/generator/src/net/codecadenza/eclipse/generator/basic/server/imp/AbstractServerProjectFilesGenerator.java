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

import net.codecadenza.eclipse.generator.basic.server.IServerProjectFilesGenerator;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Abstract base class for generators that create server-related source and configuration files
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractServerProjectFilesGenerator implements IServerProjectFilesGenerator {
	protected Project project;

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.basic.server.IServerProjectFilesGenerator#createDataSource()
	 */
	@Override
	public String createDataSource() {
		return "";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.basic.server.IServerProjectFilesGenerator#getVendorWebXMLName()
	 */
	@Override
	public String getVendorWebXMLName() {
		return "";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.basic.server.IServerProjectFilesGenerator#createVendorWebXML()
	 */
	@Override
	public String createVendorWebXML() {
		return "";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.basic.server.IServerProjectFilesGenerator#createDeploymentStructureXML()
	 */
	@Override
	public String createDeploymentStructureXML() {
		return "";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.basic.server.IServerProjectFilesGenerator#createEJBJarXML()
	 */
	@Override
	public String createEJBJarXML() {
		final var b = new StringBuilder();
		b.append("<?xml version=\"1.0\" encoding=\"" + UTF_8 + "\"?>\n");
		b.append("<ejb-jar xmlns=\"https://jakarta.ee/xml/ns/jakartaee\"\n");
		b.append("\txmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n");
		b.append("\txsi:schemaLocation=\"https://jakarta.ee/xml/ns/jakartaee ");
		b.append("https://jakarta.ee/xml/ns/jakartaee/ejb-jar_4_0.xsd\"\n");
		b.append("\tversion=\"4.0\">\n\n");
		b.append("\t<assembly-descriptor>\n\n");
		b.append("\t\t<application-exception>\n");
		b.append("\t\t\t<exception-class>net.codecadenza.runtime.repository.ConcurrentEntityModificationException");
		b.append("</exception-class>\n");
		b.append("\t\t\t<rollback>true</rollback>\n");
		b.append("\t\t</application-exception>\n\n");
		b.append("\t\t<application-exception>\n");
		b.append("\t\t\t<exception-class>net.codecadenza.runtime.repository.DuplicateCollectionEntryException");
		b.append("</exception-class>\n");
		b.append("\t\t\t<rollback>true</rollback>\n");
		b.append("\t\t</application-exception>\n\n");
		b.append("\t\t<application-exception>\n");
		b.append("\t\t\t<exception-class>net.codecadenza.runtime.transport.RemoteOperationException</exception-class>\n");
		b.append("\t\t\t<rollback>true</rollback>\n");
		b.append("\t\t</application-exception>\n\n");
		b.append("\t\t<application-exception>\n");
		b.append("\t\t\t<exception-class>net.codecadenza.runtime.search.exception.GeneralSearchException</exception-class>\n");
		b.append("\t\t\t<rollback>true</rollback>\n");
		b.append("\t\t</application-exception>\n\n");
		b.append("\t\t<application-exception>\n");
		b.append("\t\t\t<exception-class>net.codecadenza.runtime.repository.UniqueConstraintViolationException");
		b.append("</exception-class>\n");
		b.append("\t\t\t<rollback>false</rollback>\n");
		b.append("\t\t</application-exception>\n\n");
		b.append("\t\t<application-exception>\n");
		b.append("\t\t\t<exception-class>net.codecadenza.runtime.validation.PropertyConstraintViolationException");
		b.append("</exception-class>\n");
		b.append("\t\t\t<rollback>true</rollback>\n");
		b.append("\t\t</application-exception>\n\n");
		b.append("\t\t<application-exception>\n");
		b.append("\t\t\t<exception-class>net.codecadenza.runtime.exchange.DataExportException</exception-class>\n");
		b.append("\t\t\t<rollback>true</rollback>\n");
		b.append("\t\t</application-exception>\n\n");
		b.append("\t\t<application-exception>\n");
		b.append("\t\t\t<exception-class>net.codecadenza.runtime.exchange.DataImportException</exception-class>\n");
		b.append("\t\t\t<rollback>true</rollback>\n");
		b.append("\t\t</application-exception>\n\n");
		b.append("\t</assembly-descriptor>\n\n");
		b.append("</ejb-jar>\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.basic.server.IServerProjectFilesGenerator#createWebXML()
	 */
	@Override
	public String createWebXML() {
		final var b = new StringBuilder();
		b.append("<?xml version=\"1.0\" encoding=\"" + UTF_8 + "\"?>\n");
		b.append("<web-app xmlns=\"https://jakarta.ee/xml/ns/jakartaee\"\n");
		b.append("\txmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n");
		b.append("\txsi:schemaLocation=\"https://jakarta.ee/xml/ns/jakartaee ");
		b.append("https://jakarta.ee/xml/ns/jakartaee/web-app_6_0.xsd\"\n");
		b.append("\tversion=\"6.0\">\n");
		b.append("</web-app>\n");

		return b.toString();
	}

}

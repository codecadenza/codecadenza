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
package net.codecadenza.eclipse.generator.basic.project;

import static net.codecadenza.eclipse.shared.Constants.QUOTE;
import static net.codecadenza.eclipse.shared.Constants.UTF_8;

import net.codecadenza.eclipse.model.db.DBNamingUtil;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.Namespace;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.PersistenceProviderEnumeration;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Generator for the configuration files of a project
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ProjectFilesGenerator {
	private final Project project;

	/**
	 * Constructor
	 * @param project
	 */
	public ProjectFilesGenerator(Project project) {
		this.project = project;
	}

	/**
	 * Create the beans.xml file
	 * @return the generated content
	 */
	public String createBeansXML() {
		final var b = new StringBuilder();
		b.append("<?xml version=\"1.0\" encoding=\"" + UTF_8 + "\"?>\n");
		b.append("<beans xmlns=\"https://jakarta.ee/xml/ns/jakartaee\"\n");
		b.append("\txmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n");
		b.append("\txsi:schemaLocation=\"https://jakarta.ee/xml/ns/jakartaee https://jakarta.ee/xml/ns/jakartaee/beans_4_1.xsd\"\n");
		b.append("\tversion=\"4.1\"\n");
		b.append("\tbean-discovery-mode=\"all\">\n");
		b.append("</beans>\n");

		return b.toString();
	}

	/**
	 * Create the application.properties file
	 * @param artifactType
	 * @return the generated content
	 */
	public String createPropertiesFile(BuildArtifactType artifactType) {
		final var b = new StringBuilder();
		boolean addBackendProperties = false;
		boolean addRepositoryFolderProperty = false;

		if (artifactType == BuildArtifactType.SERVER || project.hasJSFOrVaadinClient()) {
			addBackendProperties = true;
			addRepositoryFolderProperty = true;
		}

		if (!project.isJavaSEApplication() && (project.hasEclipseClient() || project.hasJavaFXClient() || project.hasSwingClient())) {
			b.append("codecadenza.application.transport.secret=D!spA93l%1xx2rGpu1L32o?ksl9vpTxH\n");

			if (artifactType == BuildArtifactType.SERVER)
				if (project.getApplicationLogOnDTO() != null)
					b.append("codecadenza.application.transport.authentication.enabled=true\n");
				else
					b.append("codecadenza.application.transport.authentication.enabled=false\n");
		}
		else
			addRepositoryFolderProperty = true;

		if (addRepositoryFolderProperty)
			b.append("codecadenza.application.document-repository-folder=\n");

		if (addBackendProperties) {
			b.append("codecadenza.application.file-exchange-folder=\n");
			b.append("codecadenza.application.logging-level=4\n");
		}

		if (project.artifactExists(BuildArtifactType.INTEGRATION_IMP_REST)
				&& ((project.isSpringBootApplication() && project.hasAngularClient()) || project.isJakartaEEApplication())) {
			b.append("codecadenza.application.rest.access-control-origin=*\n");
			b.append("codecadenza.application.rest.access-control-methods=GET, POST, PUT, DELETE, OPTIONS, HEAD\n");
			b.append("codecadenza.application.rest.access-control-max-age=6000\n");
			b.append("codecadenza.application.rest.access-control-headers=");
			b.append("accept, authorization, content-type, filename, origin, path");

			if (project.isSpringBootApplication())
				b.append(", password, username");

			b.append("\n");
		}

		if (project.isSpringBootApplication()) {
			final String schemaName = project.getDatabase().getSchemaName();
			final String catalogName = project.getDatabase().getCatalogName();
			boolean addSpringBootProperties = false;
			boolean addBeanDefinitionOverriding = false;

			if (project.hasJSFOrVaadinClient()) {
				addSpringBootProperties = true;
				addBeanDefinitionOverriding = true;

				if (project.hasJSFClient())
					b.append("joinfaces.primefaces.theme=nova-light\n");
			}
			else if (artifactType == BuildArtifactType.SERVER)
				addSpringBootProperties = true;

			if (addSpringBootProperties) {
				if (project.getIntegrationModuleByArtifact(BuildArtifactType.INTEGRATION_IMP_KAFKA) != null) {
					b.append("codecadenza.application.kafka.consumer.address=localhost:9092\n");
					b.append("codecadenza.application.kafka.consumer.auto-offset-reset=latest\n");
					b.append("codecadenza.application.kafka.consumer.enable-auto-commit=true\n");
					b.append("codecadenza.application.kafka.producer.address=localhost:9092\n");
				}

				if (project.getIntegrationModuleByArtifact(BuildArtifactType.INTEGRATION_IMP_JMS) != null) {
					b.append("spring.artemis.host=localhost\n");
					b.append("spring.artemis.port=61616\n");
					b.append("spring.artemis.user=root\n");
					b.append("spring.artemis.password=root\n");
				}

				b.append("spring.datasource.driver-class-name=" + project.getDataSource().getDriverName() + "\n");
				b.append("spring.datasource.url=" + project.getDataSource().getConnectionURL() + "\n");
				b.append("spring.datasource.username=" + project.getDataSource().getUserName() + "\n");
				b.append("spring.datasource.password=" + project.getDataSource().getPassword() + "\n");

				if (schemaName != null && !schemaName.isEmpty())
					b.append("spring.datasource.hikari.schema=" + schemaName + "\n");

				if (catalogName != null && !catalogName.isEmpty())
					b.append("spring.datasource.hikari.catalog=" + catalogName + "\n");

				b.append("server.servlet.context-path=/" + project.getCode() + "\n");

				if (project.hasAngularClient() || project.hasJSFOrVaadinClient()) {
					b.append("# Check if the maximum file size must be reduced!\n");
					b.append("spring.servlet.multipart.max-file-size=" + Integer.MAX_VALUE + "\n");
					b.append("spring.servlet.multipart.max-request-size=" + Integer.MAX_VALUE + "\n");
				}

				if (project.hasJSFClient()) {
					// The default values are too small for performing a file upload in JSF
					b.append("server.tomcat.max-part-count=100\n");
					b.append("server.tomcat.max-part-header-size=2048\n");
				}

				if (addBeanDefinitionOverriding)
					b.append("spring.main.allow-bean-definition-overriding=true\n");
			}
		}

		return b.toString();
	}

	/**
	 * Create the persistence.xml file
	 * @return the generated content
	 */
	public String createPersistenceXML() {
		final var b = new StringBuilder();
		b.append("<?xml version=\"1.0\" encoding=\"" + UTF_8 + "\"?>\n");
		b.append("<persistence xmlns=\"https://jakarta.ee/xml/ns/persistence\"\n");
		b.append("\txmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n");
		b.append("\txsi:schemaLocation=\"https://jakarta.ee/xml/ns/persistence ");
		b.append("https://jakarta.ee/xml/ns/persistence/persistence_3_2.xsd\"\n");
		b.append("\tversion=\"3.2\">\n");

		// Define the default persistence unit name in order to use it in other libraries!
		b.append("\t<persistence-unit name=\"_default\" ");

		if (project.isJavaSEApplication())
			b.append("transaction-type=\"RESOURCE_LOCAL\" ");

		b.append(">\n");

		if (project.isJakartaEEApplication())
			b.append("\t\t<jta-data-source>" + project.getDataSourceJNDIName() + "</jta-data-source>\n");
		else if (project.isJavaSEApplication())
			for (final Namespace ns : project.getDomainNamespace().getChildNamespaces())
				for (final JavaType type : ns.getJavaTypes()) {
					// An enum doesn't have to be added!
					if (type.isEnum())
						continue;

					b.append("\t\t<class>" + ns.toString() + "." + type.getName() + "</class>\n");
				}

		if (project.isJavaSEApplication() || project.getPersistenceProvider() == PersistenceProviderEnumeration.ECLIPSELINK) {
			b.append("\t\t<!-- Converters for fields that are mapped to a UUID -->\n");
			b.append("\t\t<class>net.codecadenza.runtime.jpa.converter.UUIDByteArrayConverter</class>\n");
			b.append("\t\t<class>net.codecadenza.runtime.jpa.converter.UUIDStringConverter</class>\n");
			b.append("\t\t<!-- Converters for element collections that use a set -->\n");
			b.append("\t\t<class>net.codecadenza.runtime.jpa.converter.element.set.BigDecimalSetToStringConverter</class>\n");
			b.append("\t\t<class>net.codecadenza.runtime.jpa.converter.element.set.CharacterSetToStringConverter</class>\n");
			b.append("\t\t<class>net.codecadenza.runtime.jpa.converter.element.set.DateSetToStringConverter</class>\n");
			b.append("\t\t<class>net.codecadenza.runtime.jpa.converter.element.set.DoubleSetToStringConverter</class>\n");
			b.append("\t\t<class>net.codecadenza.runtime.jpa.converter.element.set.FloatSetToStringConverter</class>\n");
			b.append("\t\t<class>net.codecadenza.runtime.jpa.converter.element.set.GregorianCalendarSetToStringConverter</class>\n");
			b.append("\t\t<class>net.codecadenza.runtime.jpa.converter.element.set.IntegerSetToStringConverter</class>\n");
			b.append("\t\t<class>net.codecadenza.runtime.jpa.converter.element.set.LocalDateSetToStringConverter</class>\n");
			b.append("\t\t<class>net.codecadenza.runtime.jpa.converter.element.set.LocalDateTimeSetToStringConverter</class>\n");
			b.append("\t\t<class>net.codecadenza.runtime.jpa.converter.element.set.LongSetToStringConverter</class>\n");
			b.append("\t\t<class>net.codecadenza.runtime.jpa.converter.element.set.StringSetToStringConverter</class>\n");
			b.append("\t\t<class>net.codecadenza.runtime.jpa.converter.element.set.UUIDSetToStringConverter</class>\n");
			b.append("\t\t<!-- Converters for element collections that use a list -->\n");
			b.append("\t\t<class>net.codecadenza.runtime.jpa.converter.element.list.BigDecimalListToStringConverter</class>\n");
			b.append("\t\t<class>net.codecadenza.runtime.jpa.converter.element.list.CharacterListToStringConverter</class>\n");
			b.append("\t\t<class>net.codecadenza.runtime.jpa.converter.element.list.DateListToStringConverter</class>\n");
			b.append("\t\t<class>net.codecadenza.runtime.jpa.converter.element.list.DoubleListToStringConverter</class>\n");
			b.append("\t\t<class>net.codecadenza.runtime.jpa.converter.element.list.FloatListToStringConverter</class>\n");
			b.append("\t\t<class>net.codecadenza.runtime.jpa.converter.element.list.GregorianCalendarListToStringConverter</class>\n");
			b.append("\t\t<class>net.codecadenza.runtime.jpa.converter.element.list.IntegerListToStringConverter</class>\n");
			b.append("\t\t<class>net.codecadenza.runtime.jpa.converter.element.list.LocalDateListToStringConverter</class>\n");
			b.append("\t\t<class>net.codecadenza.runtime.jpa.converter.element.list.LocalDateTimeListToStringConverter</class>\n");
			b.append("\t\t<class>net.codecadenza.runtime.jpa.converter.element.list.LongListToStringConverter</class>\n");
			b.append("\t\t<class>net.codecadenza.runtime.jpa.converter.element.list.StringListToStringConverter</class>\n");
			b.append("\t\t<class>net.codecadenza.runtime.jpa.converter.element.list.UUIDListToStringConverter</class>\n");
		}

		b.append("\t\t<properties>\n");

		if (project.isJavaSEApplication() && project.getPersistenceProvider() == PersistenceProviderEnumeration.ECLIPSELINK) {
			b.append("\t\t\t<property name=\"jakarta.persistence.jdbc.url\" value=\"");
			b.append(project.getDataSource().getConnectionURL() + "\"/>\n");
			b.append("\t\t\t<property name=\"jakarta.persistence.jdbc.user\" value=\"");
			b.append(project.getDataSource().getUserName() + "\"/>\n");
			b.append("\t\t\t<property name=\"jakarta.persistence.jdbc.password\" value=\"");
			b.append(project.getDataSource().getPassword() + "\"/>\n");
		}

		// Add all persistence unit properties
		project.getPersistenceUnitProperties().forEach(
				property -> b.append("\t\t\t<property name=\"" + property.getName() + "\" value=\"" + property.getValue() + "\"/>\n"));

		b.append("\t\t</properties>\n");
		b.append("\t</persistence-unit>\n");
		b.append("</persistence>");

		return b.toString();
	}

	/**
	 * Create the orm.xml file
	 * @return the generated content
	 */
	public String createORMXML() {
		final var b = new StringBuilder();
		final String schemaName = project.getDatabase().getSchemaName();
		final String catalogName = project.getDatabase().getCatalogName();

		b.append("<?xml version=\"1.0\" encoding=\"" + UTF_8 + "\"?>\n");
		b.append("<entity-mappings xmlns=\"https://jakarta.ee/xml/ns/persistence/orm\"\n");
		b.append("\txmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n");
		b.append("\txsi:schemaLocation=\"https://jakarta.ee/xml/ns/persistence/orm ");
		b.append("https://jakarta.ee/xml/ns/persistence/orm/orm_3_1.xsd\"\n");
		b.append("\tversion=\"3.1\">\n");
		b.append("\t<persistence-unit-metadata>\n");
		b.append("\t\t<persistence-unit-defaults>\n");

		if (schemaName != null && !schemaName.isEmpty()) {
			b.append("\t\t\t<schema>");
			b.append(DBNamingUtil.convertToDatabase(schemaName, project.getDatabase()).replace(QUOTE, "&quot;"));
			b.append("</schema>\n");
		}

		if (catalogName != null && !catalogName.isEmpty()) {
			b.append("\t\t\t<catalog>");
			b.append(DBNamingUtil.convertToDatabase(catalogName, project.getDatabase()).replace(QUOTE, "&quot;"));
			b.append("</catalog>\n");
		}

		b.append("\t\t</persistence-unit-defaults>\n");
		b.append("\t</persistence-unit-metadata>\n");
		b.append("</entity-mappings>\n");

		return b.toString();
	}

}

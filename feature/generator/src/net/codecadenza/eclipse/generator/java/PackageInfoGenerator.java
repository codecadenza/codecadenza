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
package net.codecadenza.eclipse.generator.java;

import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Generator for package-info files
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class PackageInfoGenerator {
	/**
	 * Prevent instantiation
	 */
	private PackageInfoGenerator() {

	}

	/**
	 * Create the package-info file for a given package
	 * @param project
	 * @param packageName
	 * @param artifactType
	 * @return the generated content
	 */
	public static String createPackagInfoFile(Project project, String packageName, BuildArtifactType artifactType) {
		final var b = new StringBuilder();
		b.append("@XmlSchema\n");
		b.append("(\n");
		b.append("\tnamespace=\"" + project.getXmlNamespace() + "\",\n");
		b.append("\telementFormDefault=XmlNsForm.");

		// If the application is deployed on Wildfly the invocation of document-style SOAP services will fail if the option
		// XmlNsForm.QUALIFIED is selected!
		if (project.isDeployedOnJBoss() && artifactType == BuildArtifactType.INTEGRATION_SEI_SOAP)
			b.append("UNQUALIFIED");
		else
			b.append("QUALIFIED");

		b.append(",\n");
		b.append("\txmlns=\n");
		b.append("\t{\n");
		b.append("\t\t@XmlNs(prefix=\"" + project.getXmlNamespacePrefix());
		b.append("\", namespaceURI=\"" + project.getXmlNamespace() + "\"),\n");
		b.append("\t\t@XmlNs(prefix=\"xs\", namespaceURI=\"http://www.w3.org/2001/XMLSchema\")\n");
		b.append("\t}\n");
		b.append(")\n");
		b.append("package " + packageName + ";\n\n");
		b.append("import jakarta.xml.bind.annotation.XmlNs;\n");
		b.append("import jakarta.xml.bind.annotation.XmlNsForm;\n");
		b.append("import jakarta.xml.bind.annotation.XmlSchema;");

		return b.toString();
	}

}

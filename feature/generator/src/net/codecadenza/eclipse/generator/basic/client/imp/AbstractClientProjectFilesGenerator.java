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
package net.codecadenza.eclipse.generator.basic.client.imp;

import static net.codecadenza.eclipse.shared.Constants.APP_LOGON_EXCEPTION_NAME;
import static net.codecadenza.eclipse.shared.Constants.GENERATED_ELEMENT_ANNOTATION;
import static net.codecadenza.eclipse.shared.Constants.PACK_SERVICE_LOCATOR_RAP;
import static net.codecadenza.eclipse.shared.Constants.SECURITY_MANAGER;

import net.codecadenza.eclipse.generator.basic.client.IClientProjectFilesGenerator;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Abstract base class for generators that create basic client source files
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractClientProjectFilesGenerator implements IClientProjectFilesGenerator {
	public static final String APPLICATION_COMMENT = "Main class of this application";
	public static final String CHANGE_PWD_COMMENT = "Dialog for changing the user password";
	public static final String CLIENT_CONSTANTS_COMMENT = "Class that holds global constants";
	public static final String LOG_ON_DLG_COMMENT = "Dialog for entering the user credentials";
	public static final String SECURITY_MANAGER_COMMENT = "The security manager of this application";

	protected final Project project;
	protected final String annotationForGeneratedElement;

	/**
	 * Constructor
	 * @param project
	 */
	protected AbstractClientProjectFilesGenerator(Project project) {
		this.project = project;
		this.annotationForGeneratedElement = project.isProtectManualChanges() ? "@Generated\n" : "";
	}

	/**
	 * Create a {@link JavaFile} in the root package of the GUI artifact
	 * @param fileName the name of the file
	 * @param content the file content
	 * @param comment the comment
	 * @return a new {@link JavaFile}
	 */
	protected JavaFile createJavaSourceFile(String fileName, String content, String comment) {
		return createJavaSourceFile(fileName, content, project.getClientNamespace().toString(), comment);
	}

	/**
	 * Create a {@link JavaFile} in the given package of the GUI artifact
	 * @param fileName the name of the file
	 * @param content the file content
	 * @param packageName the fully-qualified name of the package the file should be added to
	 * @param comment the comment
	 * @return a new {@link JavaFile}
	 */
	protected JavaFile createJavaSourceFile(String fileName, String content, String packageName, String comment) {
		final var javaFile = new JavaFile(project, BuildArtifactType.GUI, fileName, packageName);
		javaFile.setComment(comment);
		javaFile.setContent(content);

		return javaFile;
	}

	/**
	 * @return the generated content
	 */
	protected String createInitalSecurityManager() {
		final var b = new StringBuilder();
		b.append("import net.codecadenza.runtime.richclient.transport.*;\n");

		if (project.isProtectManualChanges())
			b.append("import " + GENERATED_ELEMENT_ANNOTATION + ";\n");

		if (project.hasRAPClient())
			b.append("import " + PACK_SERVICE_LOCATOR_RAP + ";\n");

		b.append("\n");
		b.append("public class " + SECURITY_MANAGER + "\n");
		b.append("{\n");
		b.append("/**\n");
		b.append(" * Private constructor\n");
		b.append(" */\n");
		b.append(annotationForGeneratedElement);
		b.append("private " + SECURITY_MANAGER + "()\n");
		b.append("{\n\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Perform login\n");
		b.append(" * @param serviceLocatorDTO\n");
		b.append(" * @throws " + APP_LOGON_EXCEPTION_NAME + " if login has failed\n");
		b.append(" */\n");
		b.append(annotationForGeneratedElement);
		b.append("public static void logOn(ServiceLocatorDTO serviceLocatorDTO)\n");
		b.append("{\n");
		b.append("// Initialize service locator\n");
		b.append("ServiceLocator.initialize(serviceLocatorDTO);\n");
		b.append("}\n\n");
		b.append("}\n");

		return b.toString();
	}

}

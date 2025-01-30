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

import java.util.Collections;
import java.util.List;
import net.codecadenza.eclipse.generator.basic.client.IClientProjectFilesGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.common.AngularContentFormatter;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.Role;

/**
 * <p>
 * Generator for basic source and configuration files that are necessary for Angular applications
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class AngularClientProjectFilesGenerator implements IClientProjectFilesGenerator {
	private final Project project;

	/**
	 * Constructor
	 * @param project
	 */
	public AngularClientProjectFilesGenerator(Project project) {
		this.project = project;
	}

	/**
	 * Create the environment.ts file
	 * @param productionMode flag that controls if the file should be used either for testing or production
	 * @return the generated content
	 */
	public String createEnvironmentFile(boolean productionMode) {
		final var formatter = new AngularContentFormatter();
		formatter.addLine("export const environment = {");
		formatter.increaseIndent();
		formatter.addLine("production: " + productionMode + ",");
		formatter.addLine("SERVICE_URL: 'http://localhost:8080/" + project.getCode() + "/rest/',");
		formatter.addLine("LOCALE: 'EN'");
		formatter.decreaseIndent();
		formatter.addLine("};");
		formatter.addBlankLine();

		return formatter.getContent();
	}

	/**
	 * Create the content of the source file that contains all roles as enumeration literals
	 * @return the generated content
	 */
	public String createRoleEnum() {
		final var formatter = new AngularContentFormatter();
		final int numberOfLiterals = project.getRoles().size();
		int literalIndex = 0;

		formatter.addBlockComment("Enumeration of all roles");
		formatter.addLine("export enum RoleEnum {");
		formatter.increaseIndent();

		for (final Role role : project.getRoles()) {
			literalIndex++;

			if (literalIndex == numberOfLiterals)
				formatter.addLine(role.getName());
			else
				formatter.addLine(role.getName() + ",");
		}

		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.addBlankLine();

		return formatter.getContent();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.basic.client.IClientProjectFilesGenerator#createSourceFiles()
	 */
	@Override
	public List<JavaFile> createSourceFiles() {
		return Collections.emptyList();
	}

}

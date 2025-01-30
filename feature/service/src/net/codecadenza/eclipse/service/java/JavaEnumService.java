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
package net.codecadenza.eclipse.service.java;

import java.util.ArrayList;
import net.codecadenza.eclipse.generator.java.EnumerationGenerator;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.service.build.ProjectBuildFactory;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;

/**
 * <p>
 * Service for enumerations
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JavaEnumService {
	private final Project project;

	/**
	 * Constructor
	 * @param project
	 */
	public JavaEnumService(Project project) {
		this.project = project;
	}

	/**
	 * Rebuild the enumeration source file
	 * @param javaEnum
	 * @throws Exception if the rebuild operation has failed
	 */
	public void rebuildEnumerationSourceFile(JavaEnum javaEnum) throws Exception {
		final String source = EnumerationGenerator.createEnumeration(javaEnum);

		final JavaFile enumFile = javaEnum.getSourceFile();
		enumFile.setContent(source);

		EclipseIDEService.createJavaFile(enumFile);

		if (!project.isJavaSEApplication())
			return;

		ProjectBuildFactory.getBuildService(project).rebuildPersistenceUnit();
	}

	/**
	 * Remove the given enumeration
	 * @param javaEnum
	 * @throws IllegalStateException if the enumeration is referenced by existing domain attributes
	 * @throws Exception if either the enumeration or the respective source file could not be deleted
	 */
	public void removeEnumeration(JavaEnum javaEnum) throws Exception {
		final var beans = new ArrayList<String>();

		for (final DomainObject b : javaEnum.getNamespace().getProject().getAllDomainObjectsOfProject(true, true))
			for (final DomainAttribute a : b.getAttributes())
				if (a.getJavaType().equals(javaEnum))
					beans.add(b.getName());

		if (!beans.isEmpty()) {
			var message = "The enumeration cannot be removed, because it is referenced\nby following domain object";

			if (beans.size() > 1)
				message = message + "s";

			message = message + ":\n";

			for (final String s : beans)
				message = message + s + "\n";

			throw new IllegalStateException(message);
		}

		EclipseIDEService.deleteSource(javaEnum.getSourceFile());

		if (javaEnum.getTypeScriptSourceFile() != null)
			EclipseIDEService.deleteWorkspaceFile(javaEnum.getTypeScriptSourceFile());

		javaEnum.getNamespace().getJavaTypes().remove(javaEnum);
	}

}

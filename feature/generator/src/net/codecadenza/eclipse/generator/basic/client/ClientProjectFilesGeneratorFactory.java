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
package net.codecadenza.eclipse.generator.basic.client;

import net.codecadenza.eclipse.generator.basic.client.imp.AngularClientProjectFilesGenerator;
import net.codecadenza.eclipse.generator.basic.client.imp.EclipseClientProjectFilesGenerator;
import net.codecadenza.eclipse.generator.basic.client.imp.JSFClientProjectFilesGenerator;
import net.codecadenza.eclipse.generator.basic.client.imp.JavaFXClientProjectFilesGenerator;
import net.codecadenza.eclipse.generator.basic.client.imp.SwingClientProjectFilesGenerator;
import net.codecadenza.eclipse.generator.basic.client.imp.VaadinClientProjectFilesGenerator;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Factory for generators that create basic client source and configuration files
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ClientProjectFilesGeneratorFactory {
	/**
	 * Prevent instantiation
	 */
	private ClientProjectFilesGeneratorFactory() {

	}

	/**
	 * Factory method to get a generator implementation depending on the selected client platform
	 * @param project
	 * @return the basic client project files generator
	 * @throws IllegalStateException if an implementation for the given client platform is not available
	 */
	public static IClientProjectFilesGenerator getGenerator(Project project) {
		if (project.hasEclipseClient())
			return new EclipseClientProjectFilesGenerator(project);
		else if (project.hasSwingClient())
			return new SwingClientProjectFilesGenerator(project);
		else if (project.hasJSFClient())
			return new JSFClientProjectFilesGenerator(project);
		else if (project.hasVaadinClient())
			return new VaadinClientProjectFilesGenerator(project);
		else if (project.hasJavaFXClient())
			return new JavaFXClientProjectFilesGenerator(project);
		else if (project.hasAngularClient())
			return new AngularClientProjectFilesGenerator(project);

		throw new IllegalStateException(
				"A generator for the client platform '" + project.getClientPlatform() + "' is not available!");
	}

}

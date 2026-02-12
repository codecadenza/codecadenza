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
package net.codecadenza.eclipse.generator.basic.server;

import net.codecadenza.eclipse.generator.basic.server.imp.PayaraServerProjectFilesGenerator;
import net.codecadenza.eclipse.generator.basic.server.imp.WildflyServerProjectFilesGenerator;
import net.codecadenza.eclipse.generator.basic.server.imp.LocalServerProjectFilesGenerator;
import net.codecadenza.eclipse.generator.basic.server.imp.TomcatServerProjectFilesGenerator;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.ServerPlatformEnumeration;

/**
 * <p>
 * Factory for generators that create basic server-related source and configuration files
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ServerProjectFilesGeneratorFactory {
	/**
	 * Constructor
	 */
	private ServerProjectFilesGeneratorFactory() {

	}

	/**
	 * Factory method to get a generator implementation depending on the selected server platform
	 * @param project
	 * @return the basic server project files generator
	 * @throws IllegalStateException if an implementation for the given server platform is not available
	 */
	public static IServerProjectFilesGenerator getGenerator(Project project) {
		final ServerPlatformEnumeration serverPlatform = project.getServerPlatform();

		if (serverPlatform == ServerPlatformEnumeration.WILDFLY)
			return new WildflyServerProjectFilesGenerator(project);
		else if (serverPlatform == ServerPlatformEnumeration.PAYARA)
			return new PayaraServerProjectFilesGenerator(project);
		else if (serverPlatform == ServerPlatformEnumeration.TOMCAT)
			return new TomcatServerProjectFilesGenerator(project);
		else if (serverPlatform == ServerPlatformEnumeration.NONE)
			return new LocalServerProjectFilesGenerator(project);

		throw new IllegalStateException("A generator for the platform '" + serverPlatform + "' is not available!");
	}

}

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
package net.codecadenza.eclipse.generator.basic.integration;

import net.codecadenza.eclipse.generator.basic.integration.imp.JMSIntegrationProjectFilesGenerator;
import net.codecadenza.eclipse.generator.basic.integration.imp.KafkaIntegrationProjectFilesGenerator;
import net.codecadenza.eclipse.generator.basic.integration.imp.RESTIntegrationProjectFilesGenerator;
import net.codecadenza.eclipse.generator.basic.integration.imp.RMIIntegrationProjectFilesGenerator;
import net.codecadenza.eclipse.generator.basic.integration.imp.SOAPIntegrationProjectFilesGenerator;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.IntegrationModule;
import net.codecadenza.eclipse.model.project.IntegrationTechnology;

/**
 * <p>
 * Factory for generators that create basic integration source and configuration files
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class IntegrationProjectFilesGeneratorFactory {
	/**
	 * Prevent instantiation
	 */
	private IntegrationProjectFilesGeneratorFactory() {

	}

	/**
	 * Factory method to get a generator implementation depending on the selected integration technology
	 * @param module
	 * @param artifactType
	 * @return the basic integration project files generator
	 * @throws IllegalStateException if an implementation for the given integration technology is not available
	 */
	public static IIntegrationProjectFilesGenerator getGenerator(IntegrationModule module, BuildArtifactType artifactType) {
		if (module.getTechnology() == IntegrationTechnology.SOAP)
			return new SOAPIntegrationProjectFilesGenerator(module, artifactType);
		else if (module.getTechnology() == IntegrationTechnology.REST)
			return new RESTIntegrationProjectFilesGenerator(module, artifactType);
		else if (module.getTechnology() == IntegrationTechnology.RMI)
			return new RMIIntegrationProjectFilesGenerator(module, artifactType);
		else if (module.getTechnology() == IntegrationTechnology.KAFKA)
			return new KafkaIntegrationProjectFilesGenerator(module, artifactType);
		else if (module.getTechnology() == IntegrationTechnology.JMS)
			return new JMSIntegrationProjectFilesGenerator(module, artifactType);

		throw new IllegalStateException("A generator for the technology '" + module.getTechnology() + "' is not available!");
	}

}

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
package net.codecadenza.eclipse.service.build.imp.module;

import net.codecadenza.eclipse.model.project.BuildArtifact;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.service.build.imp.module.imp.BoundaryBuildModule;
import net.codecadenza.eclipse.service.build.imp.module.imp.ClientInterfaceBuildModule;
import net.codecadenza.eclipse.service.build.imp.module.imp.DTOBuildModule;
import net.codecadenza.eclipse.service.build.imp.module.imp.DataExchangeBuildModule;
import net.codecadenza.eclipse.service.build.imp.module.imp.DomainBuildModule;
import net.codecadenza.eclipse.service.build.imp.module.imp.FacadeBuildModule;
import net.codecadenza.eclipse.service.build.imp.module.imp.GUIBuildModule;
import net.codecadenza.eclipse.service.build.imp.module.imp.IntegrationClientBuildModule;
import net.codecadenza.eclipse.service.build.imp.module.imp.IntegrationImpBuildModule;
import net.codecadenza.eclipse.service.build.imp.module.imp.IntegrationSEIBuildModule;
import net.codecadenza.eclipse.service.build.imp.module.imp.MasterBuildModule;
import net.codecadenza.eclipse.service.build.imp.module.imp.RepositoryBuildModule;
import net.codecadenza.eclipse.service.build.imp.module.imp.SeleniumTestBuildModule;
import net.codecadenza.eclipse.service.build.imp.module.imp.ServerBuildModule;
import net.codecadenza.eclipse.service.build.imp.module.imp.ServiceBuildModule;
import net.codecadenza.eclipse.service.build.imp.module.imp.SharedBuildModule;

/**
 * <p>
 * Factory for build modules
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class BuildModuleFactory {
	/**
	 * Prevent instantiation
	 */
	private BuildModuleFactory() {

	}

	/**
	 * @param buildArtifact
	 * @param artifactType
	 * @return the build module
	 * @throws IllegalStateException if a build module for the given artifact type is not available
	 */
	public static IBuildModule getBuildModule(BuildArtifact buildArtifact, BuildArtifactType artifactType) {
		if (artifactType == BuildArtifactType.DOMAIN)
			return new DomainBuildModule(buildArtifact);
		else if (artifactType == BuildArtifactType.REPOSITORY)
			return new RepositoryBuildModule(buildArtifact);
		else if (artifactType == BuildArtifactType.DTO)
			return new DTOBuildModule(buildArtifact);
		else if (artifactType == BuildArtifactType.BOUNDARY)
			return new BoundaryBuildModule(buildArtifact);
		else if (artifactType == BuildArtifactType.FACADE)
			return new FacadeBuildModule(buildArtifact);
		else if (artifactType == BuildArtifactType.DATA_EXCHANGE)
			return new DataExchangeBuildModule(buildArtifact);
		else if (artifactType == BuildArtifactType.SERVICE)
			return new ServiceBuildModule(buildArtifact);
		else if (artifactType == BuildArtifactType.CLIENT_INTERFACE)
			return new ClientInterfaceBuildModule(buildArtifact);
		else if (artifactType == BuildArtifactType.GUI)
			return new GUIBuildModule(buildArtifact);
		else if (artifactType == BuildArtifactType.SERVER)
			return new ServerBuildModule(buildArtifact);
		else if (artifactType == BuildArtifactType.MASTER)
			return new MasterBuildModule(buildArtifact);
		else if (artifactType == BuildArtifactType.INTEGRATION_IMP_SOAP || artifactType == BuildArtifactType.INTEGRATION_IMP_REST
				|| artifactType == BuildArtifactType.INTEGRATION_IMP_RMI || artifactType == BuildArtifactType.INTEGRATION_IMP_KAFKA
				|| artifactType == BuildArtifactType.INTEGRATION_IMP_JMS)
			return new IntegrationImpBuildModule(buildArtifact, artifactType);
		else if (artifactType == BuildArtifactType.INTEGRATION_SEI_SOAP || artifactType == BuildArtifactType.INTEGRATION_SEI_REST
				|| artifactType == BuildArtifactType.INTEGRATION_SEI_RMI || artifactType == BuildArtifactType.INTEGRATION_SEI_KAFKA
				|| artifactType == BuildArtifactType.INTEGRATION_SEI_JMS)
			return new IntegrationSEIBuildModule(buildArtifact, artifactType);
		else if (artifactType == BuildArtifactType.INTEGRATION_CLIENT_SOAP
				|| artifactType == BuildArtifactType.INTEGRATION_CLIENT_REST || artifactType == BuildArtifactType.INTEGRATION_CLIENT_RMI
				|| artifactType == BuildArtifactType.INTEGRATION_CLIENT_KAFKA || artifactType == BuildArtifactType.INTEGRATION_CLIENT_JMS)
			return new IntegrationClientBuildModule(buildArtifact, artifactType);
		else if (artifactType == BuildArtifactType.SELENIUM_TEST)
			return new SeleniumTestBuildModule(buildArtifact, artifactType);
		else if (artifactType == BuildArtifactType.SHARED)
			return new SharedBuildModule(buildArtifact);

		throw new IllegalStateException("A build module for the artifact type '" + artifactType + "' is not available!");
	}

}

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
package net.codecadenza.eclipse.service.build.imp.module.imp;

import java.util.List;
import net.codecadenza.eclipse.model.project.BuildArtifact;

/**
 * <p>
 * Build module for shared artifacts
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SharedBuildModule extends AbstractBuildModule {
	/**
	 * Constructor
	 * @param buildArtifact
	 */
	public SharedBuildModule(BuildArtifact buildArtifact) {
		super(buildArtifact);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.service.build.imp.module.imp.AbstractBuildModule#getPackages()
	 */
	@Override
	public List<String> getPackages() {
		final List<String> packages = super.getPackages();
		packages.add(project.getDomainNamespace().toString());

		// Add all domain object packages as the generator will place all enumerations of the domain model into this artifact!
		project.getDomainNamespace().getChildNamespaces().forEach(namespace -> packages.add(namespace.toString()));

		return packages;
	}

}

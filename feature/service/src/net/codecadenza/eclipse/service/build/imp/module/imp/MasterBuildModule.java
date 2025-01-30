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

import static net.codecadenza.eclipse.shared.Constants.MAVEN_NATURE_ID;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import net.codecadenza.eclipse.model.project.BuildArtifact;
import org.eclipse.jdt.core.IClasspathEntry;

/**
 * <p>
 * Build module for master artifacts
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class MasterBuildModule extends AbstractBuildModule {
	/**
	 * Constructor
	 * @param buildArtifact
	 */
	public MasterBuildModule(BuildArtifact buildArtifact) {
		super(buildArtifact);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.service.build.imp.module.imp.AbstractBuildModule#getNatures()
	 */
	@Override
	public List<String> getNatures() {
		final var natures = new ArrayList<String>();
		natures.add(MAVEN_NATURE_ID);

		return natures;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.service.build.imp.module.imp.AbstractBuildModule#getClassPathEntries(java.lang.String)
	 */
	@Override
	public HashSet<IClasspathEntry> getClassPathEntries(String projectName) {
		return new HashSet<>();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.service.build.imp.module.imp.AbstractBuildModule#getFolders()
	 */
	@Override
	public List<String> getFolders() {
		return new ArrayList<>();
	}

}

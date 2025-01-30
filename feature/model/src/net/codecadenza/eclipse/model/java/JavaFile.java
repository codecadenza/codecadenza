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
package net.codecadenza.eclipse.model.java;

import static net.codecadenza.eclipse.shared.Constants.JAVA_RESOURCE_SUFFIX;

import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.WorkspaceFile;

/**
 * <p>
 * Internal representation of Java source files
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JavaFile extends WorkspaceFile {
	protected Project project;
	protected String comment;
	protected String packageName;
	protected String sourceFolder;
	protected String className;

	/**
	 * Constructor
	 * @param project
	 * @param type
	 * @param className
	 * @param packageName
	 */
	public JavaFile(Project project, BuildArtifactType type, String className, String packageName) {
		super(project.getTargetProjectName(type), className + JAVA_RESOURCE_SUFFIX, project.getSourceFolder());

		this.project = project;
		this.packageName = packageName;
		this.sourceFolder = project.getSourceFolder();
		this.className = className;
	}

	/**
	 * Constructor
	 * @param namespace
	 * @param type
	 * @param className
	 * @param packageName
	 */
	public JavaFile(Namespace namespace, BuildArtifactType type, String className, String packageName) {
		this(namespace.getProject(), type, className, packageName);
	}

	/**
	 * Constructor
	 * @param projectName
	 * @param name
	 * @param path
	 */
	public JavaFile(String projectName, String name, String path) {
		super(projectName, name, path);
	}

	/**
	 * @return the project this Java source file belongs to
	 */
	public Project getProject() {
		return project;
	}

	/**
	 * @return the comment
	 */
	public String getComment() {
		return comment;
	}

	/**
	 * @param comment
	 */
	public void setComment(String comment) {
		this.comment = comment;
	}

	/**
	 * @return the package name
	 */
	public String getPackageName() {
		return packageName;
	}

	/**
	 * @param packageName
	 */
	public void setPackageName(String packageName) {
		this.packageName = packageName;
	}

	/**
	 * @return the source folder
	 */
	public String getSourceFolder() {
		return sourceFolder;
	}

	/**
	 * @param sourceFolder
	 */
	public void setSourceFolder(String sourceFolder) {
		this.sourceFolder = sourceFolder;
	}

	/**
	 * @return the class name
	 */
	public String getClassName() {
		return className;
	}

}

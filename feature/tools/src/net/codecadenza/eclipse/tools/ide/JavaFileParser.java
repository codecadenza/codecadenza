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
package net.codecadenza.eclipse.tools.ide;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import net.codecadenza.eclipse.model.java.JavaFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.jdt.core.IAnnotation;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IField;
import org.eclipse.jdt.core.IImportContainer;
import org.eclipse.jdt.core.IImportDeclaration;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IMethod;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.Signature;

/**
 * <p>
 * Utility class for preventing manual changes in Java source files from being lost when rebuilding them
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JavaFileParser {
	private final HashMap<String, String> importMap = new HashMap<>();
	private final HashMap<String, String> declarationMap = new HashMap<>();
	private final HashMap<String, String> customizedMethodMap = new HashMap<>();
	private final HashMap<String, String> customizedSubClassMap = new HashMap<>();
	private final HashMap<String, String> customizedConstructorMap = new HashMap<>();
	private final Collection<String> protectedMethods = new ArrayList<>();
	private final Collection<String> protectedSubClasses = new ArrayList<>();
	private final String projectName;
	private final String sourceFolder;
	private final String packageName;
	private final String className;

	/**
	 * Constructor
	 * @param javaFile
	 */
	public JavaFileParser(JavaFile javaFile) {
		this.projectName = javaFile.getProjectName();
		this.sourceFolder = javaFile.getSourceFolder();
		this.packageName = javaFile.getPackageName();
		this.className = javaFile.getName();
	}

	/**
	 * Parses the Java file, given in the constructor.<br>
	 * The method extracts all import declarations, all global field declarations,<br>
	 * the methods annotated with '@Customized', all constructors annotated with '@Customized'<br>
	 * and all 'protected' methods, that are not annotated with '@Customized' or '@Generated'.
	 * @throws Exception if an internal error has occurred
	 */
	public void parse() throws Exception {
		final IWorkspaceRoot wsRoot = ResourcesPlugin.getWorkspace().getRoot();
		final IProject project = wsRoot.getProject(projectName);
		final IFolder folder = project.getFolder(sourceFolder);
		final IJavaProject javaProject = JavaCore.create(project);
		final IPackageFragmentRoot fragmentRoot = javaProject.getPackageFragmentRoot(folder);
		final IPackageFragment fragment = fragmentRoot.getPackageFragment(packageName);
		final ICompilationUnit unit = fragment.getCompilationUnit(className);

		if (!unit.exists())
			return;

		for (final IJavaElement e : unit.getChildren()) {
			final int outerType = e.getElementType();

			if (outerType == IJavaElement.TYPE) {
				final var type = (IType) e;

				for (final IJavaElement i : type.getChildren()) {
					final int innerType = i.getElementType();

					if (innerType == IJavaElement.METHOD) {
						final var method = (IMethod) i;
						AnnotationType methodType = AnnotationType.Protected;

						for (final IAnnotation an : method.getAnnotations()) {
							if (an.getElementName().equals(AnnotationType.Customized.name())) {
								methodType = AnnotationType.Customized;
								break;
							}
							else if (an.getElementName().equals(AnnotationType.Generated.name()))
								methodType = AnnotationType.Generated;
						}

						if (method.isConstructor() && methodType == AnnotationType.Customized) {
							var key = method.getElementName() + "(";

							for (int a = 0; a < method.getParameterNames().length; a++)
								key = key + Signature.toString(method.getParameterTypes()[a]) + " " + method.getParameterNames()[a] + ", ";

							if (method.getParameterNames().length > 0)
								key = key.substring(0, key.length() - 2);

							key = key + ")";

							customizedConstructorMap.put(key, method.getSource());
							continue;
						}

						if (methodType == AnnotationType.Customized) {
							var key = Signature.toString(method.getReturnType()) + " " + method.getElementName() + "(";

							for (int a = 0; a < method.getParameterNames().length; a++)
								key = key + Signature.toString(method.getParameterTypes()[a]) + " " + method.getParameterNames()[a] + ", ";

							if (method.getParameterNames().length > 0)
								key = key.substring(0, key.length() - 2);

							key = key + ")";

							customizedMethodMap.put(key, method.getSource());
						}
						else if (methodType == AnnotationType.Protected)
							protectedMethods.add(method.getSource());
					}
					else if (innerType == IJavaElement.FIELD) {
						final var field = (IField) i;
						AnnotationType classType = AnnotationType.Protected;

						for (final IAnnotation an : field.getAnnotations())
							if (an.getElementName().equals(AnnotationType.Generated.name())) {
								classType = AnnotationType.Generated;
								break;
							}

						if (classType != AnnotationType.Generated) {
							final var key = Signature.toString(field.getTypeSignature()) + " " + field.getElementName();
							declarationMap.put(key, field.getSource());
						}
					}
					else if (innerType == IJavaElement.TYPE) {
						final var subClass = (IType) i;
						AnnotationType classType = AnnotationType.Protected;

						for (final IAnnotation an : subClass.getAnnotations()) {
							if (an.getElementName().equals(AnnotationType.Customized.name())) {
								classType = AnnotationType.Customized;
								break;
							}
							else if (an.getElementName().equals(AnnotationType.Generated.name()))
								classType = AnnotationType.Generated;
						}

						if (classType == AnnotationType.Customized) {
							final String key = subClass.getElementName();
							customizedSubClassMap.put(key, subClass.getSource());
						}
						else if (classType == AnnotationType.Protected)
							protectedSubClasses.add(subClass.getSource());
					}
				}
			}
			else if (outerType == IJavaElement.IMPORT_CONTAINER) {
				final var s = (IImportContainer) e;

				for (final IJavaElement i : s.getChildren()) {
					final int type = i.getElementType();

					if (type == IJavaElement.IMPORT_DECLARATION) {
						final var d = (IImportDeclaration) i;
						importMap.put(d.getSource(), null);
					}
				}
			}
		}
	}

	/**
	 * Get the import map<br>
	 * <b>e.g.: </b><i>import java.lang.*;</i><br>
	 * <b>key: </b><i>import java.lang.*;</i><br>
	 * <b>value: </b><i>null</i>
	 * @return a map with all import declarations
	 */
	public Map<String, String> getImportMap() {
		return importMap;
	}

	/**
	 * Get the global field declaration map containing non @Generated declarations<br>
	 * <b>e.g.: </b><i>private String a;</i><br>
	 * <b>key: </b><i>String a</i><br>
	 * <b>value: </b><i>private String a;</i>
	 * @return a map with all global field declarations
	 */
	public Map<String, String> getDeclarationMap() {
		return declarationMap;
	}

	/**
	 * Get the customized method map<br>
	 * <b>e.g.: </b><i>public String toString(long a, int b){}</i><br>
	 * <b>key: </b><i>String toString(long a, int b)</i><br>
	 * <b>value: </b>the complete source code of the method, including annotations and comment
	 * @return a map with all customized methods
	 */
	public Map<String, String> getCustomizedMethodMap() {
		return customizedMethodMap;
	}

	/**
	 * Get the protected methods including the complete source code of a method, including annotations and comment
	 * @return a collection with all protected methods
	 */
	public Collection<String> getProtectedMethods() {
		return protectedMethods;
	}

	/**
	 * Get the customized constructor map<br>
	 * <b>e.g.: </b><i>public JavaFileParser(String projectName, String sourceFolder, String packageName, String fileName){}</i><br>
	 * <b>key: </b><i>JavaFileParser(String projectName, String sourceFolder, String packageName, String fileName)</i><br>
	 * <b>value: </b>the complete source code of the constructor, including annotations and comment
	 * @return a map with all customized methods
	 */
	public Map<String, String> getCustomizedConstructorMap() {
		return customizedConstructorMap;
	}

	/**
	 * Get the customized subclass map<br>
	 * <b>e.g.: </b><i>public class JavaFileParser implements Serializable{}</i><br>
	 * <b>key: </b><i>JavaFileParser</i><br>
	 * <b>value: </b>the complete source code of the subclass, including annotations and comment
	 * @return a map with all customized subclasses
	 */
	public Map<String, String> getCustomizedSubClassMap() {
		return customizedSubClassMap;
	}

	/**
	 * Get the protected subclasses including the complete source code of a subclass, including annotations and comment
	 * @return a collection with all protected subclasses
	 */
	public Collection<String> getProtectedSubClasses() {
		return protectedSubClasses;
	}

}

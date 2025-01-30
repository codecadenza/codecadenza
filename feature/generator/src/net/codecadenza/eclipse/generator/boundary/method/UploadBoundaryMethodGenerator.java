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
package net.codecadenza.eclipse.generator.boundary.method;

import java.util.HashSet;
import java.util.Set;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.common.IMethodGeneratorUtility;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.MethodParameter;

/**
 * <p>
 * Generator for boundary methods for upload operations
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class UploadBoundaryMethodGenerator extends BasicBoundaryMethodGenerator {
	/**
	 * Constructor
	 * @param method
	 * @param utility
	 * @param parentGenerator
	 */
	public UploadBoundaryMethodGenerator(BoundaryMethod method, IMethodGeneratorUtility utility,
			AbstractJavaSourceGenerator parentGenerator) {
		super(method, utility, parentGenerator);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.boundary.method.BasicBoundaryMethodGenerator#createMethodBody()
	 */
	@Override
	public String createMethodBody() {
		final var b = new StringBuilder();
		final DomainObject domainObject = method.getBoundaryBean().getDomainObject();
		final String domainObjectName = domainObject.getLowerCaseName();
		final DomainAttribute lobAttribute = method.getDomainAttribute();
		final JavaType type = lobAttribute.getJavaType();
		final String getter = createLOBAttributeGetter(domainObject, domainObjectName, lobAttribute);
		final String setter = createLOBAttributeSetter(domainObject, domainObjectName, lobAttribute);
		var idParamName = "";
		var lobParamName = "";

		boolean isFirstParam = true;

		for (final MethodParameter p : method.getMethodParameters()) {
			if (isFirstParam) {
				idParamName = p.getName();
				isFirstParam = false;
			}
			else {
				lobParamName = p.getName();
				break;
			}
		}

		b.append("if(path == null || path.isEmpty())\n");
		b.append("throw new IllegalArgumentException(\"Path must not be null or empty!\");\n\n");
		b.append("final var file = new File(" + lobParamName + ");\n\n");
		b.append("if(!file.exists())\n");
		b.append("throw new IllegalArgumentException(\"File doesn't exist!\");\n\n");
		b.append("// Find and attach object\n");
		b.append("final " + domainObject.getName() + " " + domainObjectName + " = ");
		b.append(getRepositoryName() + method.getServiceMethod().getName() + "(");
		b.append(idParamName + ", true);\n\n");
		b.append("try\n");
		b.append("{\n");

		if (type.isType(JavaType.BYTE_ARRAY))
			b.append(setter + "(FileUtil.getBytesFromFile(file));\n\n");
		else if (type.isType(JavaType.BYTE_OBJ_ARRAY))
			b.append(setter + "(FileUtil.convertToByteArray(FileUtil.getBytesFromFile(file)));\n\n");
		else if (type.isString()) {
			// Because of the given type it should be a document reference!
			b.append("String repositoryPath = " + getter + ";\n\n");
			b.append("if(repositoryPath == null || repositoryPath.isEmpty())\n");
			b.append("repositoryPath = FileUtil.getUniqueFileName(" + domainObject.getName() + ".class.getSimpleName());\n\n");
			b.append("FileUtil.copyFile(file, new File(repositoryPath));\n");
			b.append(setter + "(repositoryPath);\n\n");
		}

		// We must not delete the original file if the application doesn't run in a managed environment!
		if (!project.isJavaSEApplication()) {
			b.append("// Delete temporary file after data has been saved in respective field!\n");
			b.append("Files.delete(file.toPath());\n");
		}
		else
			b.append("tr.commit();\n");

		b.append("}\n");
		b.append("catch (final Exception e)\n");
		b.append("{\n");
		b.append("throw new FileOperationException(e);\n");
		b.append("}\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.boundary.method.BasicBoundaryMethodGenerator#getImports()
	 */
	@Override
	public Set<String> getImports() {
		final var imports = new HashSet<>(super.getImports());
		imports.add("import java.io.*;");
		imports.add("import net.codecadenza.runtime.file.*;");

		if (!project.isJavaSEApplication())
			imports.add("import java.nio.file.*;");

		return imports;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.boundary.method.BasicBoundaryMethodGenerator#getInterfaceImports()
	 */
	@Override
	public Set<String> getInterfaceImports() {
		return new HashSet<>();
	}

}

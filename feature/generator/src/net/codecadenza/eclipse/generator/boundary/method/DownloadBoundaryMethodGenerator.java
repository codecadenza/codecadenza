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
import net.codecadenza.eclipse.model.domain.AttributeTagEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.MethodParameter;

/**
 * <p>
 * Generator for boundary methods for download operations
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DownloadBoundaryMethodGenerator extends BasicBoundaryMethodGenerator {
	private final DomainAttribute lobAttribute;
	private final MethodParameter param;

	/**
	 * Constructor
	 * @param method
	 * @param utility
	 * @param parentGenerator
	 */
	public DownloadBoundaryMethodGenerator(BoundaryMethod method, IMethodGeneratorUtility utility,
			AbstractJavaSourceGenerator parentGenerator) {
		super(method, utility, parentGenerator);

		this.lobAttribute = method.getDomainAttribute();
		this.param = method.getFirstParameter();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.boundary.method.BasicBoundaryMethodGenerator#createComment()
	 */
	@Override
	public String createComment() {
		final var b = new StringBuilder();
		b.append(method.generateBeginOfJavadocComment());
		b.append(" * @param " + param.getName() + "\n");
		b.append(" * @return the fully qualified path name of the file to download\n");
		b.append(" */\n");

		return b.toString();
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
		final String getter = createLOBAttributeGetter(domainObject, domainObjectName, lobAttribute);
		final JavaType type = lobAttribute.getJavaType();
		final DomainAttribute nameAttribute = lobAttribute.getDomainObject().getAllAttributes().stream()
				.filter(attr -> attr.getTag() == AttributeTagEnumeration.DOCUMENT_NAME).findFirst().orElse(null);

		if (type.isByteArray()) {
			b.append("File file = null;\n");
			b.append("FileOutputStream fout = null;\n\n");
		}

		b.append("// Find and attach object\n");
		b.append("final " + domainObject.getName() + " " + domainObjectName + " = ");
		b.append(getRepositoryName() + method.getServiceMethod().getName() + "(");
		b.append(param.getName() + ", true);\n\n");

		if (type.isByteArray()) {
			b.append("try\n");
			b.append("{\n");

			if (lobAttribute.getDomainAttributeValidator().isNullable()) {
				b.append("if(" + getter + " == null)\n");
				b.append("return null;\n\n");
			}

			if (nameAttribute != null) {
				b.append("var suffix = \"\";\n");
				b.append("final char delimiter = '.';\n");
				b.append("final String fileName = " + domainObjectName + "." + nameAttribute.getGetterName() + ";\n\n");
				b.append("if(fileName.lastIndexOf(delimiter) != -1 && fileName.lastIndexOf(delimiter) != fileName.length() - 1)\n");
				b.append("suffix = delimiter + fileName.substring(fileName.lastIndexOf(delimiter) + 1);\n\n");
			}

			b.append("file = File.createTempFile(" + domainObject.getName() + ".class.getSimpleName(), ");

			if (nameAttribute != null)
				b.append("suffix");
			else
				b.append("\".dat\"");

			b.append(");\n\n");
			b.append("fout = new FileOutputStream(file);\n");

			if (type.isType(JavaType.BYTE_ARRAY))
				b.append("fout.write(" + getter + ");\n\n");
			else if (type.isType(JavaType.BYTE_OBJ_ARRAY))
				b.append("fout.write(FileUtil.convertToByteArray(" + getter + "));\n\n");
		}

		if (type.isString() && lobAttribute.getMinFieldLength().isEmpty()) {
			b.append("if(" + getter + ".isEmpty())\n");
			b.append("return null;\n\n");
		}

		if (type.isByteArray())
			b.append("return file.getAbsolutePath();\n");
		else
			b.append("return " + getter + ";\n");

		if (type.isByteArray()) {
			b.append("}\n");
			b.append("catch (final Exception e)\n");
			b.append("{\n");
			b.append("throw new FileOperationException(e);\n");
			b.append("}\n");
			b.append("finally\n");
			b.append("{\n");
			b.append("if(fout != null)\n");
			b.append("try\n");
			b.append("{\n");
			b.append("fout.close();\n");
			b.append("}\n");
			b.append("catch (final IOException e)\n");
			b.append("{\n");

			parentGenerator.addWarningLog(b, "Could not close file output stream!", "e");

			b.append("}\n");
			b.append("}\n");
		}

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.boundary.method.BasicBoundaryMethodGenerator#getImports()
	 */
	@Override
	public Set<String> getImports() {
		final var imports = new HashSet<>(super.getImports());
		final JavaType type = lobAttribute.getJavaType();

		if (type.isByteArray()) {
			imports.add("import java.io.*;");
			imports.add("import net.codecadenza.runtime.file.*;");
		}

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

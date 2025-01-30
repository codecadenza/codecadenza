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
package net.codecadenza.eclipse.generator.facade.method;

import static net.codecadenza.eclipse.shared.Constants.REPO_METHOD_NAME_DELETE_ENTITY;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import net.codecadenza.eclipse.generator.boundary.method.DeleteBoundaryMethodGenerator;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.common.IMethodGeneratorUtility;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.domain.AttributeTagEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.java.MethodParameter;
import net.codecadenza.eclipse.model.repository.RepositoryMethod;
import net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration;

/**
 * <p>
 * Generator for facade methods that perform a delete operation
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DeleteFacadeMethodGenerator extends DeleteBoundaryMethodGenerator {
	private final DomainObject domainObject;
	private final List<DomainAttribute> lobAttributeList;
	private final boolean hasDocRef;

	/**
	 * Constructor
	 * @param method
	 * @param utility
	 * @param parentGenerator
	 */
	public DeleteFacadeMethodGenerator(BoundaryMethod method, IMethodGeneratorUtility utility,
			AbstractJavaSourceGenerator parentGenerator) {
		super(method, utility, parentGenerator);

		this.domainObject = method.getBoundaryBean().getDomainObject();
		this.lobAttributeList = domainObject.getAllLobAttributes();
		this.hasDocRef = lobAttributeList.stream().anyMatch(a -> a.getTag() == AttributeTagEnumeration.DOCUMENT_REF);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.boundary.method.RemoveBoundaryMethodGenerator#createMethodBody()
	 */
	@Override
	public String createMethodBody() {
		final var b = new StringBuilder();
		final var domainObjectName = "domainObject";
		final MethodParameter param = method.getFirstParameter();
		final RepositoryMethod finderMethod = method.getBoundaryBean().getRepository()
				.getMethodByType(RepositoryMethodTypeEnumeration.FIND_EXISTING);

		b.append("// Find and attach object\n");
		b.append(domainObject.getName() + " " + domainObjectName + " = " + finderMethod.getName() + "(");
		b.append(param.getName() + ");\n\n");

		// Save all file paths before deleting the domain object
		int pathIndex = 1;

		for (final DomainAttribute attr : lobAttributeList) {
			if (attr.getTag() != AttributeTagEnumeration.DOCUMENT_REF)
				continue;

			b.append("final String path" + pathIndex + " = " + createLOBAttributeGetter(domainObject, domainObjectName, attr) + ";\n");

			pathIndex++;
		}

		if (hasDocRef)
			b.append("\n");

		b.append(REPO_METHOD_NAME_DELETE_ENTITY + "(" + domainObjectName + ");\n");

		// Finally we have to delete all referenced files!
		pathIndex = 1;

		if (hasDocRef) {
			b.append("\n// Delete referenced file(s)!\n");
			b.append("try\n");
			b.append("{\n");

			for (final DomainAttribute attr : lobAttributeList) {
				if (attr.getTag() != AttributeTagEnumeration.DOCUMENT_REF)
					continue;

				if (pathIndex > 1)
					b.append("\n");

				if (attr.getMinFieldLength().isEmpty())
					b.append("if(path" + pathIndex + " != null && !path" + pathIndex + ".isEmpty())\n");

				b.append("Files.delete(new File(path" + pathIndex + ").toPath());\n");

				pathIndex++;
			}

			b.append("}\n");
			b.append("catch (final Exception e)\n");
			b.append("{\n");

			parentGenerator.addErrorLog(b, "Error while deleting file!", "e");

			b.append("}\n");
		}

		if (addTransactionManagement)
			b.append("\ntr.commit();\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.boundary.method.RemoveBoundaryMethodGenerator#getImports()
	 */
	@Override
	public Set<String> getImports() {
		final var imports = new HashSet<>(super.getImports());

		if (hasDocRef) {
			imports.add("import java.io.*;");
			imports.add("import java.nio.file.*;");
		}

		return imports;
	}

}

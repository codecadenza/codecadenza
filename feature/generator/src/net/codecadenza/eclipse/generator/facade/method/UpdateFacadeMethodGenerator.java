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

import static net.codecadenza.eclipse.shared.Constants.EXISTING_OBJ_PREFIX;

import java.util.HashSet;
import java.util.Set;
import net.codecadenza.eclipse.generator.boundary.method.UpdateBoundaryMethodGenerator;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.common.IMethodGeneratorUtility;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.repository.RepositoryMethod;

/**
 * <p>
 * Generator for facade methods that perform an update operation
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class UpdateFacadeMethodGenerator extends UpdateBoundaryMethodGenerator {
	/**
	 * Constructor
	 * @param method
	 * @param utility
	 * @param parentGenerator
	 */
	public UpdateFacadeMethodGenerator(BoundaryMethod method, IMethodGeneratorUtility utility,
			AbstractJavaSourceGenerator parentGenerator) {
		super(method, utility, parentGenerator);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.boundary.method.MergeBoundaryMethodGenerator#createMergeCall(java.lang.String)
	 */
	@Override
	public String createMergeCall(String domainObjectName) {
		final var b = new StringBuilder();
		final RepositoryMethod repositoryMethod = (RepositoryMethod) method.getServiceMethod();
		final JavaType returnType = method.getReturnType();
		final var checkParam = repositoryMethod.addUniqueCheck() ? ", true" : "";

		b.append("\n");

		if (returnType instanceof DTOBean) {
			b.append(domainObjectName + " = " + repositoryMethod.getName() + "(" + domainObjectName + checkParam + ", true);\n\n");

			if (addTransactionManagement)
				b.append("tr.commit();\n\n");

			b.append("return " + returnObjectName + ";\n");
		}
		else {
			b.append(getRepositoryName() + repositoryMethod.getName() + "(" + domainObjectName + checkParam + ", false);\n");

			if (addTransactionManagement)
				b.append("\ntr.commit();\n");
		}

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.boundary.method.MergeBoundaryMethodGenerator#createMethodBody()
	 */
	@Override
	public String createMethodBody() {
		final var b = new StringBuilder();
		final Project project = dtoBean.getNamespace().getProject();
		String domainObjName = returnObjectName;

		if (!project.isJavaSEApplication()) {
			domainObjName = EXISTING_OBJ_PREFIX + dtoBean.getDomainObject().getUpperCaseName();

			// Invoke the finder method
			b.append(createFindMethodCall(domainObjName, dtoBean, returnObjectName));
		}

		b.append(new DomainObjectInlineConversionGenerator(method.getMethodType(), dtoBean, domainObjName, returnObjectName)
				.createConversion());

		// Invoke the merge method
		b.append(createMergeCall(domainObjName));

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.boundary.method.MergeBoundaryMethodGenerator#getImports()
	 */
	@Override
	public Set<String> getImports() {
		final var converter = new DomainObjectInlineConversionGenerator(method.getMethodType(), dtoBean, "", "");

		final var imports = new HashSet<>(generatorUtility.getImports());
		imports.addAll(converter.getImports());

		if (method.addUniqueCheck())
			imports.add("import net.codecadenza.runtime.repository.*;");

		return imports;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.boundary.method.MergeBoundaryMethodGenerator#getInterfaceImports()
	 */
	@Override
	public Set<String> getInterfaceImports() {
		return getImports();
	}

}

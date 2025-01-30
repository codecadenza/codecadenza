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
import net.codecadenza.eclipse.generator.dto.DTOInlineConversionGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.MethodParameter;
import net.codecadenza.eclipse.model.repository.RepositoryMethod;

/**
 * <p>
 * Generator for boundary methods that perform an update operation
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class UpdateBoundaryMethodGenerator extends BasicBoundaryMethodGenerator {
	private static final String OBJ_PREFIX_EXISTING = "existing";

	protected DTOBean dtoBean;
	protected String returnObjectName;

	/**
	 * Constructor
	 * @param method
	 * @param utility
	 * @param parentGenerator
	 */
	public UpdateBoundaryMethodGenerator(BoundaryMethod method, IMethodGeneratorUtility utility,
			AbstractJavaSourceGenerator parentGenerator) {
		super(method, utility, parentGenerator);

		for (final MethodParameter p : method.getMethodParameters())
			if (p.getType() instanceof final DTOBean dto) {
				this.dtoBean = dto;
				this.returnObjectName = p.getName();
				break;
			}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.boundary.method.BasicBoundaryMethodGenerator#createComment()
	 */
	@Override
	public String createComment() {
		final var b = new StringBuilder();
		b.append(method.generateBeginOfJavadocComment());

		method.getMethodParameters()
				.forEach(param -> b.append(" * @param " + param.getName() + " the " + domainObjectLabel + " to update\n"));

		if (method.addUniqueCheck())
			b.append(" * @throws UniqueConstraintViolationException if a unique constraint check has failed\n");

		if (!validationExceptionComment.isEmpty())
			b.append(validationExceptionComment);

		if (!method.getReturnType().isVoid())
			b.append(" * @return the updated " + domainObjectLabel + " object\n");

		b.append(" */\n");

		return b.toString();
	}

	/**
	 * Create the merge method call
	 * @param domainObjectName
	 * @return the generated content
	 */
	public String createMergeCall(String domainObjectName) {
		final var b = new StringBuilder();
		final var repositoryMethod = (RepositoryMethod) method.getServiceMethod();
		final var checkParam = repositoryMethod.addUniqueCheck() ? ", true" : "";
		final JavaType returnType = method.getReturnType();

		if (returnType instanceof DTOBean) {
			b.append("\n" + domainObjectName + " = " + getRepositoryName() + repositoryMethod.getName());
			b.append("(" + domainObjectName + checkParam + ", true);\n\n");

			// Change the values of the version and the last update fields of the DTO if they exist
			for (final DTOBeanAttribute attr : dtoBean.getAttributes()) {
				if (attr.getDomainAttribute() == null || attr.getAssociation() != null)
					continue;

				if (attr.getDomainAttribute().isTrackVersion() || attr.getDomainAttribute().isSetDateOnUpdate()) {
					final String setter = attr.getSetterName();
					final String getter = attr.getDomainAttribute().getGetterName();

					b.append(returnObjectName + "." + setter + "(" + domainObjectName + "." + getter + ");\n");
				}
			}

			if (addTransactionManagement)
				b.append("\ntr.commit();\n\n");

			b.append("return " + returnObjectName + ";\n");
		}
		else {
			b.append("\n" + getRepositoryName() + repositoryMethod.getName() + "(" + domainObjectName + checkParam + ", false);\n");

			if (addTransactionManagement)
				b.append("\ntr.commit();\n");
		}

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.boundary.method.BasicBoundaryMethodGenerator#createMethodBody()
	 */
	@Override
	public String createMethodBody() {
		final var b = new StringBuilder();
		final String defaultName = dtoBean.getDomainObject().getLowerCaseName();
		final String domainObjName = returnObjectName.equals(defaultName) ? OBJ_PREFIX_EXISTING + dtoBean.getDomainObject().getName()
				: defaultName;
		final var converter = new DTOInlineConversionGenerator(method.getMethodType(), dtoBean, domainObjName, returnObjectName);

		// Invoke the finder method
		b.append(createFindMethodCall(domainObjName, dtoBean, returnObjectName));
		b.append(converter.addAttributeSetters());
		b.append(createMergeCall(domainObjName));

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.boundary.method.BasicBoundaryMethodGenerator#getImports()
	 */
	@Override
	public Set<String> getImports() {
		final var imports = new HashSet<>(super.getImports());

		if (!validationExceptionImport.isEmpty())
			imports.add(validationExceptionImport);

		if (method.addUniqueCheck())
			imports.add("import net.codecadenza.runtime.repository.*;");

		final var converter = new DTOInlineConversionGenerator(BoundaryMethodTypeEnumeration.UPDATE, dtoBean, "name",
				returnObjectName);
		imports.addAll(converter.getImports());

		return imports;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.boundary.method.BasicBoundaryMethodGenerator#getInterfaceImports()
	 */
	@Override
	public Set<String> getInterfaceImports() {
		final var imports = new HashSet<>(super.getInterfaceImports());

		if (!validationExceptionImport.isEmpty())
			imports.add(validationExceptionImport);

		if (method.addUniqueCheck())
			imports.add("import net.codecadenza.runtime.repository.*;");

		return imports;
	}

}

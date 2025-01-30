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

import static net.codecadenza.eclipse.shared.Constants.PARAM_LOGGED_ON_USER;

import java.util.HashSet;
import java.util.Set;
import net.codecadenza.eclipse.generator.common.IMethodGeneratorUtility;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodDataFetchType;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainTagEnumeration;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration;
import net.codecadenza.eclipse.model.java.MethodParameter;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.repository.RepositoryMethod;

/**
 * <p>
 * Generator utility for facade methods
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class FacadeMethodGeneratorUtility implements IMethodGeneratorUtility {
	protected BoundaryMethod method;
	protected Project project;

	/**
	 * Constructor
	 * @param method
	 */
	public FacadeMethodGeneratorUtility(BoundaryMethod method) {
		this.method = method;
		this.project = method.getBoundaryBean().getNamespace().getProject();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.IMethodGeneratorUtility#getRepositoryName()
	 */
	@Override
	public String getRepositoryName() {
		return "";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.IMethodGeneratorUtility#getImports()
	 */
	@Override
	public Set<String> getImports() {
		final var imports = new HashSet<String>();
		final DomainAttribute additionalFilterAttribute = method.getAdditionalFilterAttribute();

		if (method.getReturnTypeModifier() != JavaTypeModifierEnumeration.NONE)
			imports.add("import java.util.*;");

		if (method.getServiceMethod().getReturnType() != null && !method.getServiceMethod().getReturnType().isPrimitive()
				&& method.getReturnType().getNamespace() != null) {
			if (method.getReturnType() instanceof final DTOBean dto) {
				if (method.useDTOReturnType())
					imports.add("import " + dto.getNamespace().toString() + ".*;");
				else
					imports.add("import " + dto.getDomainObject().getNamespace().toString() + ".*;");
			}
			else
				imports.add("import " + method.getServiceMethod().getReturnType().getNamespace().toString() + ".*;");
		}

		method.getMethodParameters().forEach(param -> {
			if (!param.getType().isPrimitive() && param.getType().getNamespace() != null) {
				if (param.getType() instanceof final DTOBean dto)
					imports.add("import " + dto.getDomainObject().getNamespace().toString() + ".*;");
				else
					imports.add("import " + param.getType().getNamespace().toString() + ".*;");
			}

			if (param.getModifier() != JavaTypeModifierEnumeration.NONE)
				imports.add("import java.util.*;");
		});

		if (additionalFilterAttribute != null && additionalFilterAttribute.getJavaType().getNamespace() != null)
			imports.add("import " + additionalFilterAttribute.getJavaType().getNamespace().toString() + ".*;");

		return imports;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.IMethodGeneratorUtility#getMethodSignature(boolean, boolean)
	 */
	@Override
	public String getMethodSignature(boolean addQualifier, boolean useParameterNames) {
		var b = new StringBuilder();
		String returnType = method.getReturnType().getName();

		if (method.getReturnType() instanceof final DTOBean dto && !method.useDTOReturnType())
			returnType = dto.getDomainObject().getName();

		if (addQualifier)
			b.append("public ");

		if (method.getReturnTypeModifier() == JavaTypeModifierEnumeration.NONE)
			b.append(returnType + " " + method.getName() + "(");
		else
			b.append(method.getReturnTypeModifier().toString() + "<" + returnType + "> " + method.getName() + "(");

		if (method.getMethodType() == BoundaryMethodTypeEnumeration.SEARCH
				|| method.getMethodType() == BoundaryMethodTypeEnumeration.COUNT
				|| method.getMethodType() == BoundaryMethodTypeEnumeration.FIND_ALL) {
			boolean hasParam = false;

			if (method.getMethodType() != BoundaryMethodTypeEnumeration.FIND_ALL) {
				b.append("SearchDTO searchObj");

				hasParam = true;
			}

			if (method.getDataFetchType() == BoundaryMethodDataFetchType.CLIENT) {
				final DomainAttribute clientPkAttr = project.getDomainObjectByTag(DomainTagEnumeration.CLIENT).getPKAttribute();
				final String clientParamName = project.getDomainObjectByTag(DomainTagEnumeration.CLIENT).getLowerCaseName()
						+ clientPkAttr.getUpperCaseName();

				if (hasParam)
					b.append(", ");

				b.append(clientPkAttr.getJavaType().getName() + " " + clientParamName);
			}
			else if (method.getDataFetchType() == BoundaryMethodDataFetchType.USER) {
				final DomainAttribute userPkAttr = project.getDomainObjectByTag(DomainTagEnumeration.USER).getPKAttribute();
				final String userParamName = project.getDomainObjectByTag(DomainTagEnumeration.USER).getLowerCaseName()
						+ userPkAttr.getUpperCaseName();

				if (hasParam)
					b.append(", ");

				b.append(userPkAttr.getJavaType().getName() + " " + userParamName);
			}
		}
		else {
			for (final MethodParameter p : method.getMethodParameters()) {
				String typeName = p.getType().getName();

				if (p.getType() instanceof final DTOBean dto)
					typeName = dto.getDomainObject().getName();

				if (p.getModifier() == JavaTypeModifierEnumeration.NONE)
					b.append(typeName + " " + p.getName() + ", ");
				else
					b.append(p.getModifier().toString() + "<" + typeName + "> " + p.getName() + ", ");
			}

			if (!method.getMethodParameters().isEmpty())
				b = new StringBuilder(b.substring(0, b.length() - 2));
		}

		if ((method.getMethodType() == BoundaryMethodTypeEnumeration.SEARCH_BY_FILTER
				|| method.getMethodType() == BoundaryMethodTypeEnumeration.GET_LIST_OF_VALUES)
				&& method.getBoundaryBean().getDomainObject().isMandated()) {
			final DomainAttribute clientPkAttr = project.getDomainObjectByTag(DomainTagEnumeration.CLIENT).getPKAttribute();
			final String clientParamName = project.getDomainObjectByTag(DomainTagEnumeration.CLIENT).getLowerCaseName()
					+ clientPkAttr.getUpperCaseName();

			b.append(", " + clientPkAttr.getJavaType().getName() + " " + clientParamName);
		}

		if (method.getMethodType() == BoundaryMethodTypeEnumeration.COPY) {
			final var repositoryMethod = (RepositoryMethod) method.getServiceMethod();

			if (repositoryMethod.addUserParam()) {
				final DTOBean logOnDTO = project.getApplicationLogOnDTO();

				b.append(", " + logOnDTO.getPKAttribute().getDomainAttribute().getJavaType().getName() + " " + PARAM_LOGGED_ON_USER);
			}
		}

		b.append(")");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.IMethodGeneratorUtility#getInterfaceImports()
	 */
	@Override
	public Set<String> getInterfaceImports() {
		return getImports();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.IMethodGeneratorUtility#getLocalServices()
	 */
	@Override
	public String getLocalServices() {
		return "";
	}

}

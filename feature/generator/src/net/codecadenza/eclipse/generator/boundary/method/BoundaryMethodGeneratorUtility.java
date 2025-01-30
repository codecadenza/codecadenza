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

import static net.codecadenza.eclipse.shared.Constants.DEFAULT_REPOSITORY;
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
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration;
import net.codecadenza.eclipse.model.java.MethodParameter;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.repository.RepositoryMethod;
import net.codecadenza.eclipse.model.service.ServiceBean;

/**
 * <p>
 * Utility class for boundary method generators
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class BoundaryMethodGeneratorUtility implements IMethodGeneratorUtility {
	protected BoundaryMethod method;
	protected Project project;

	/**
	 * Constructor
	 * @param method
	 */
	public BoundaryMethodGeneratorUtility(BoundaryMethod method) {
		this.method = method;
		this.project = method.getBoundaryBean().getNamespace().getProject();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.IMethodGeneratorUtility#getRepositoryName()
	 */
	@Override
	public String getRepositoryName() {
		return DEFAULT_REPOSITORY + ".";
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

		if (method.getServiceMethod() != null && method.getServiceMethod().getReturnType() != null
				&& !method.getServiceMethod().getReturnType().isPrimitive() && method.getReturnType().getNamespace() != null)
			imports.add("import " + method.getServiceMethod().getReturnType().getNamespace().toString() + ".*;");

		if (method.getReturnType() != null && !method.getReturnType().isPrimitive() && method.getReturnType().getNamespace() != null)
			imports.add("import " + method.getReturnType().getNamespace().toString() + ".*;");

		method.getMethodParameters().forEach(param -> {
			if (!param.getType().isPrimitive() && param.getType().getNamespace() != null)
				imports.add("import " + param.getType().getNamespace().toString() + ".*;");

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
		final String returnType = method.getReturnType().getName();
		int paramCounter = 1;

		if (addQualifier)
			b.append("public ");

		if (method.getReturnTypeModifier() == JavaTypeModifierEnumeration.NONE)
			b.append(returnType + " " + method.getName() + "(");
		else
			b.append(method.getReturnTypeModifier().toString() + "<" + returnType + "> " + method.getName() + "(");

		if (method.getMethodType() == BoundaryMethodTypeEnumeration.SEARCH
				|| method.getMethodType() == BoundaryMethodTypeEnumeration.COUNT) {
			b.append("SearchDTO searchObj");

			if (method.getDataFetchType() == BoundaryMethodDataFetchType.CLIENT) {
				final DomainAttribute clientPkAttr = project.getDomainObjectByTag(DomainTagEnumeration.CLIENT).getPKAttribute();
				String clientParamName = project.getDomainObjectByTag(DomainTagEnumeration.CLIENT).getLowerCaseName()
						+ clientPkAttr.getUpperCaseName();

				if (!useParameterNames) {
					clientParamName = "param" + paramCounter;
					paramCounter++;
				}

				b.append(", ");
				b.append(clientPkAttr.getJavaType().getName() + " " + clientParamName);
			}
			else if (method.getDataFetchType() == BoundaryMethodDataFetchType.USER) {
				final DomainAttribute userPkAttr = project.getDomainObjectByTag(DomainTagEnumeration.USER).getPKAttribute();
				String userParamName = project.getDomainObjectByTag(DomainTagEnumeration.USER).getLowerCaseName()
						+ userPkAttr.getUpperCaseName();

				if (!useParameterNames) {
					userParamName = "param" + paramCounter;
					paramCounter++;
				}

				b.append(", ");
				b.append(userPkAttr.getJavaType().getName() + " " + userParamName);
			}
		}
		else {
			for (final MethodParameter p : method.getMethodParameters()) {
				final String typeName = p.getType().getName();
				String paramName = p.getName();

				if (!useParameterNames) {
					paramName = "param" + paramCounter;
					paramCounter++;
				}

				if (p.getModifier() == JavaTypeModifierEnumeration.NONE)
					b.append(typeName + " " + paramName + ", ");
				else
					b.append(p.getModifier().toString() + "<" + typeName + "> " + paramName + ", ");
			}

			if (!method.getMethodParameters().isEmpty())
				b = new StringBuilder(b.substring(0, b.length() - 2));
		}

		if ((method.getMethodType() == BoundaryMethodTypeEnumeration.SEARCH_BY_FILTER
				|| method.getMethodType() == BoundaryMethodTypeEnumeration.GET_LIST_OF_VALUES)
				&& method.getBoundaryBean().getDomainObject().isMandated()) {
			final DomainAttribute clientPkAttr = project.getDomainObjectByTag(DomainTagEnumeration.CLIENT).getPKAttribute();
			String clientParamName = project.getDomainObjectByTag(DomainTagEnumeration.CLIENT).getLowerCaseName()
					+ clientPkAttr.getUpperCaseName();

			if (!useParameterNames)
				clientParamName = "param" + paramCounter;

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
		final var imports = new HashSet<String>();
		final DomainAttribute additionalFilterAttribute = method.getAdditionalFilterAttribute();

		method.getMethodParameters().forEach(parameter -> {
			if (!parameter.getType().isPrimitive() && parameter.getType().getNamespace() != null)
				imports.add("import " + parameter.getType().getNamespace().toString() + ".*;");

			if (parameter.getModifier() != JavaTypeModifierEnumeration.NONE)
				imports.add("import java.util.*;");
		});

		if (method.getReturnTypeModifier() != JavaTypeModifierEnumeration.NONE)
			imports.add("import java.util.*;");

		if (method.getReturnType() != null && !method.getReturnType().isPrimitive() && method.getReturnType().getNamespace() != null)
			imports.add("import " + method.getReturnType().getNamespace().toString() + ".*;");

		if (additionalFilterAttribute != null && additionalFilterAttribute.getJavaType().getNamespace() != null)
			imports.add("import " + additionalFilterAttribute.getJavaType().getNamespace().toString() + ".*;");

		return imports;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.IMethodGeneratorUtility#getLocalServices()
	 */
	@Override
	public String getLocalServices() {
		final var b = new StringBuilder();
		ServiceBean bean = null;
		String beanName = DEFAULT_REPOSITORY;

		if (method.getServiceMethod() instanceof final DataExchangeMethod exchangeMethod) {
			bean = exchangeMethod.getDataExchangeServiceBean();
			beanName = bean.getLowerCaseName();
		}
		else
			bean = method.getBoundaryBean().getRepository();

		b.append("final var " + beanName + " = new " + bean.getName() + "(em);\n");

		return b.toString();
	}

}

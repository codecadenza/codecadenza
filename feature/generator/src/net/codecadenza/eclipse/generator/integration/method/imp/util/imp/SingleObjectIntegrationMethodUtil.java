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
package net.codecadenza.eclipse.generator.integration.method.imp.util.imp;

import static net.codecadenza.eclipse.model.java.JavaType.BOOL;
import static net.codecadenza.eclipse.shared.Constants.DEFAULT_REPOSITORY;
import static net.codecadenza.eclipse.shared.Constants.EXISTING_OBJ_PREFIX;

import java.util.HashSet;
import java.util.Set;
import java.util.stream.Stream;
import net.codecadenza.eclipse.generator.dto.DTOInlineConversionGenerator;
import net.codecadenza.eclipse.generator.integration.method.imp.util.AbstractIntegrationMethodUtil;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod;
import net.codecadenza.eclipse.model.integration.IntegrationMethodParameter;
import net.codecadenza.eclipse.model.repository.Repository;
import net.codecadenza.eclipse.model.repository.RepositoryMethod;
import net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration;

/**
 * <p>
 * Generator utility for facade methods that convert a single domain object into a respective data transfer object
 * </p>
 * <p>
 * Copyright 2016 (C) Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SingleObjectIntegrationMethodUtil extends AbstractIntegrationMethodUtil {
	/**
	 * Constructor
	 * @param method
	 */
	public SingleObjectIntegrationMethodUtil(AbstractIntegrationMethod method) {
		super(method);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.integration.method.imp.util.AbstractIntegrationMethodUtil#createConversionFragment()
	 */
	@Override
	public String createConversionFragment() {
		final var b = new StringBuilder();
		final var dtoBean = (DTOBean) method.getReturnType();
		final Repository repository = boundaryMethod.getBoundaryBean().getRepository();
		final String domainObjName = EXISTING_OBJ_PREFIX + dtoBean.getDomainObject().getUpperCaseName();
		final var converter = new DTOInlineConversionGenerator(type, dtoBean, domainObjName, OBJ_INSTANCE_NAME);

		// Search for an appropriate finder method
		final RepositoryMethod finderMethod = repository.getMethodByType(RepositoryMethodTypeEnumeration.FIND_EXISTING);

		if (finderMethod == null)
			throw new IllegalStateException("An appropriate repository method of type 'FIND_EXISTING' could not be found!");

		b.append(dtoBean.getDomainObject().getName() + " " + domainObjName + " = ");
		b.append(DEFAULT_REPOSITORY + "." + finderMethod.getName() + "(");

		final IntegrationMethodParameter integrationParam = method.getIntegrationParameters().stream().findFirst().orElse(null);

		if (integrationParam != null) {
			b.append(integrationParam.getName());

			if (integrationParam.getMethodParameter().getType() instanceof final DTOBean paramDTO)
				b.append("." + paramDTO.getPKAttribute().getGetterName());
		}

		if (boundaryMethod.getMethodType() == BoundaryMethodTypeEnumeration.FIND_EXISTING) {
			final Stream<IntegrationMethodParameter> paramStream = method.getIntegrationParameters().stream();
			final IntegrationMethodParameter param = paramStream.filter(p -> p.getMethodParameter().getType().getName().equals(BOOL))
					.findFirst().orElse(null);

			if (param != null)
				b.append(", " + param.getName());
		}

		b.append(");\n\n");
		b.append("if(" + domainObjName + " == null)\n");
		b.append(handleNullReturn());
		b.append("\n");
		b.append("final var " + OBJ_INSTANCE_NAME + " = new " + dtoBean.getName() + "();\n");
		b.append(converter.addReverseAttributeSetters());

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.integration.method.imp.util.AbstractIntegrationMethodUtil#getImports()
	 */
	@Override
	public Set<String> getImports() {
		final var imports = new HashSet<String>();
		final var dtoBean = (DTOBean) method.getReturnType();
		final var converter = new DTOInlineConversionGenerator(type, dtoBean, "", "");

		imports.addAll(super.getImports());
		imports.addAll(converter.getReverseImports());
		imports.add("import " + dtoBean.getDomainObject().getNamespace().toString() + ".*;");

		return imports;
	}

}

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
package net.codecadenza.eclipse.generator.integration.method.imp.rest;

import java.util.Set;
import java.util.stream.Collectors;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.integration.RESTIntegrationMethod;
import net.codecadenza.eclipse.model.java.MethodParameter;

/**
 * <p>
 * Generator for unique key check methods
 * </p>
 * <p>
 * Copyright 2020 (C) Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ExistsByUniqueKeyRESTMethodGenerator extends BasicRESTMethodGenerator {
	private final boolean hasDTOParam;

	/**
	 * Constructor
	 * @param restMethod
	 * @param parentGenerator
	 */
	public ExistsByUniqueKeyRESTMethodGenerator(RESTIntegrationMethod restMethod, AbstractJavaSourceGenerator parentGenerator) {
		super(restMethod, parentGenerator);

		this.hasDTOParam = restMethod.getBoundaryMethod().getMethodParameters().stream()
				.anyMatch(param -> param.getType() instanceof DTOBean);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.integration.method.imp.rest.BasicRESTMethodGenerator#createMethodLogic()
	 */
	@Override
	public String createMethodLogic() {
		if (!hasDTOParam)
			return super.createMethodLogic();

		final var b = new StringBuilder();
		final String objectSuffix = project.isBoundaryMode() ? "DTO" : "Entity";
		boolean firstParam = true;

		restMethod.getBoundaryMethod().getMethodParameters().stream().filter(param -> param.getType() instanceof DTOBean)
				.forEach(param -> {
					final var dto = (DTOBean) param.getType();

					b.append("final var " + param.getName() + objectSuffix + " = new ");

					// It is necessary to create either a respective DTO or a domain object for all parameters that internally reference a
					// DTO!
					if (project.isBoundaryMode())
						b.append(dto.getName());
					else
						b.append(dto.getDomainObject().getName());

					b.append("(" + param.getName() + ");\n");
				});

		b.append("\n");
		b.append("final Boolean obj = " + getServiceName() + "." + restMethod.getBoundaryMethod().getName() + "(");

		for (final MethodParameter param : restMethod.getBoundaryMethod().getMethodParameters()) {
			if (firstParam)
				firstParam = false;
			else
				b.append(", ");

			if (param.getType() instanceof DTOBean)
				b.append(param.getName() + objectSuffix);
			else
				b.append(param.getName());
		}

		b.append(");\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.integration.method.imp.rest.BasicRESTMethodGenerator#getImports()
	 */
	@Override
	public Set<String> getImports() {
		final Set<String> imports = super.getImports();

		if (!hasDTOParam || project.isBoundaryMode())
			return imports;

		final Set<String> domainObjImports = restMethod.getBoundaryMethod().getMethodParameters().stream()
				.map(MethodParameter::getType).filter(DTOBean.class::isInstance).map(DTOBean.class::cast).map(DTOBean::getDomainObject)
				.map(type -> "import " + type.getNamespace().toString() + ".*;").collect(Collectors.toSet());

		imports.addAll(domainObjImports);

		// In the case of the facade mode the DTO imports are not necessary!
		imports.removeAll(getUnnecessaryImports());

		return imports;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.integration.method.imp.rest.BasicRESTMethodGenerator#getInterfaceImports()
	 */
	@Override
	public Set<String> getInterfaceImports() {
		final Set<String> imports = super.getInterfaceImports();

		if (!hasDTOParam)
			return imports;

		imports.removeAll(getUnnecessaryImports());

		return imports;
	}

	/**
	 * @return a set containing all imports that are not necessary
	 */
	private Set<String> getUnnecessaryImports() {
		return restMethod.getBoundaryMethod().getMethodParameters().stream().map(MethodParameter::getType)
				.filter(DTOBean.class::isInstance).map(type -> "import " + type.getNamespace().toString() + ".*;")
				.collect(Collectors.toSet());
	}

}

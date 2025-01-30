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

import static net.codecadenza.eclipse.shared.Constants.APP_LOGON_EXCEPTION_NAME;

import java.util.HashSet;
import java.util.Set;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.common.IMethodGeneratorUtility;
import net.codecadenza.eclipse.generator.dto.DTOInlineConversionGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.domain.AttributeTagEnumeration;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.java.MethodParameter;
import net.codecadenza.eclipse.model.service.ServiceMethod;

/**
 * <p>
 * Generator for boundary methods that perform a logon operation
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class LogOnBoundaryMethodGenerator extends BasicBoundaryMethodGenerator {
	/**
	 * Constructor
	 * @param method
	 * @param utility
	 * @param parentGenerator
	 */
	public LogOnBoundaryMethodGenerator(BoundaryMethod method, IMethodGeneratorUtility utility,
			AbstractJavaSourceGenerator parentGenerator) {
		super(method, utility, parentGenerator);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.boundary.method.BasicBoundaryMethodGenerator#createComment()
	 */
	@Override
	public String createComment() {
		final var b = new StringBuilder();
		b.append(method.generateBeginOfJavadocComment(true));
		b.append(" * @return a data transfer object containing security information\n");
		b.append(" * @throws " + APP_LOGON_EXCEPTION_NAME + " if login has failed!\n");
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
		final var logOnDTO = (DTOBean) method.getReturnType();
		final DomainObject userDomainObject = logOnDTO.getDomainObject();
		final ServiceMethod serviceMethod = method.getServiceMethod();
		var pwdGetter = "";
		var activeGetter = "";

		for (final DTOBeanAttribute attr : logOnDTO.getAttributes()) {
			if (attr.getDomainAttribute() == null)
				continue;

			if (attr.getDomainAttribute().getTag() == AttributeTagEnumeration.USER_PASSWORD)
				pwdGetter = attr.getGetterName();
			else if (attr.getDomainAttribute().getTag() == AttributeTagEnumeration.USER_ACTIVE)
				activeGetter = attr.getGetterName();
		}

		// Get parameters. By definition, the first parameter represents the user name.
		var paramUserName = "";
		var paramPassword = "";
		boolean isFirstParam = true;

		for (final MethodParameter p : method.getMethodParameters())
			if (isFirstParam) {
				isFirstParam = false;
				paramUserName = p.getName();
			}
			else
				paramPassword = p.getName();

		b.append("final " + userDomainObject.getName() + " user = ");
		b.append(getRepositoryName() + serviceMethod.getName() + "(" + paramUserName + ");\n\n");
		b.append("if(user == null)\n");
		b.append("throw new " + APP_LOGON_EXCEPTION_NAME + "(\"User account does not exist!\");\n\n");

		if (!activeGetter.isEmpty()) {
			b.append("if(!user." + activeGetter + ")\n");
			b.append("throw new " + APP_LOGON_EXCEPTION_NAME + "(\"Account is locked!\");\n\n");
		}

		if (!project.hasJSFClient()) {
			// For JSF applications we should use a predefined security realm (JAAS) and encrypted passwords! If we do so this compare
			// will always fail!
			b.append("if(!user." + pwdGetter + ".equals(" + paramPassword + "))\n");
			b.append("throw new " + APP_LOGON_EXCEPTION_NAME + "(\"Password is not correct!\");\n\n");
		}

		b.append("final var dto = new " + logOnDTO.getName() + "();\n\n");

		final var converter = new DTOInlineConversionGenerator(method.getMethodType(), logOnDTO, "user", "dto");

		b.append(converter.addReverseAttributeSetters());
		b.append("\nreturn dto;\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.boundary.method.BasicBoundaryMethodGenerator#getImports()
	 */
	@Override
	public Set<String> getImports() {
		final var converter = new DTOInlineConversionGenerator(BoundaryMethodTypeEnumeration.LOG_ON, (DTOBean) method.getReturnType(),
				"user", "dto");

		final var imports = new HashSet<>(super.getImports());
		imports.add("import java.util.*;");
		imports.addAll(converter.getReverseImports());

		return imports;
	}

}

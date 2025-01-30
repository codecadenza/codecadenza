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
import net.codecadenza.eclipse.model.java.MethodParameter;

/**
 * <p>
 * Generator for boundary methods to change the password of a given user account
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ChangePasswordBoundaryMethodGenerator extends BasicBoundaryMethodGenerator {
	private MethodParameter paramId;
	private MethodParameter newPasswordParam;
	private MethodParameter oldPasswordParam;
	private MethodParameter confPasswordParam;

	/**
	 * Constructor
	 * @param method
	 * @param utility
	 * @param parentGenerator
	 */
	public ChangePasswordBoundaryMethodGenerator(BoundaryMethod method, IMethodGeneratorUtility utility,
			AbstractJavaSourceGenerator parentGenerator) {
		super(method, utility, parentGenerator);

		for (final MethodParameter param : method.getMethodParameters()) {
			// We assume that the parameters always appear in the same order!
			if (paramId == null) {
				this.paramId = param;
				continue;
			}

			if (this.oldPasswordParam == null) {
				this.oldPasswordParam = param;
				continue;
			}

			if (this.newPasswordParam == null) {
				this.newPasswordParam = param;
				continue;
			}

			if (this.confPasswordParam == null)
				this.confPasswordParam = param;
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.boundary.method.BasicBoundaryMethodGenerator#createComment()
	 */
	@Override
	public String createComment() {
		final var b = new StringBuilder();
		b.append(method.generateBeginOfJavadocComment(true));
		b.append(" * @throws IllegalArgumentException if input is invalid\n");
		b.append(" * @throws IllegalStateException if the password encryption algorithm does not exist\n");
		b.append(" */\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.boundary.method.BasicBoundaryMethodGenerator#getImports()
	 */
	@Override
	public Set<String> getImports() {
		final var imports = new HashSet<>(super.getImports());
		imports.add("import net.codecadenza.runtime.crypto.*;");
		imports.add("import java.security.NoSuchAlgorithmException;");

		return imports;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.boundary.method.BasicBoundaryMethodGenerator#createMethodBody()
	 */
	@Override
	public String createMethodBody() {
		final var b = new StringBuilder();
		final DomainObject domainObject = method.getBoundaryBean().getDomainObject();
		final String domainObjName = domainObject.getLowerCaseName();
		final DomainAttribute passwordAttribute = domainObject.getAllAttributes().stream()
				.filter(attr -> attr.getTag() == AttributeTagEnumeration.USER_PASSWORD).findFirst().orElse(null);

		if (passwordAttribute == null)
			throw new IllegalStateException("A properly tagged domain object attribute could not be found!");

		b.append("if(!" + newPasswordParam.getName() + ".equals(" + confPasswordParam.getName() + "))\n");
		b.append("throw new IllegalArgumentException(\"New password and confirmed password are not equal!\");\n\n");
		b.append("// Find persistent object\n");
		b.append("final " + domainObject.getName() + " " + domainObjName + " = ");
		b.append(getRepositoryName() + method.getServiceMethod().getName() + "(" + paramId.getName() + ", true);\n\n");
		b.append("try\n");
		b.append("{\n");
		b.append("if(!" + domainObjName + "." + passwordAttribute.getGetterName() + ".equals(HashGenerator.encryptSHA256(");
		b.append(oldPasswordParam.getName() + ")))\n");
		b.append("throw new IllegalArgumentException(\"Password is incorrect!\");\n\n");
		b.append(domainObjName + "." + passwordAttribute.getSetterName());
		b.append("(HashGenerator.encryptSHA256(" + newPasswordParam.getName() + "));\n");
		b.append("}\n");
		b.append("catch (final NoSuchAlgorithmException e)\n");
		b.append("{\n");
		b.append("throw new IllegalStateException(e);\n");
		b.append("}\n");

		if (addTransactionManagement)
			b.append("\ntr.commit();\n");

		return b.toString();
	}

}

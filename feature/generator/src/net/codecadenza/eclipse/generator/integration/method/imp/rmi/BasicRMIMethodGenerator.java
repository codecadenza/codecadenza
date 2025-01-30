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
package net.codecadenza.eclipse.generator.integration.method.imp.rmi;

import static net.codecadenza.eclipse.generator.integration.method.imp.util.AbstractIntegrationMethodUtil.createServiceMethodInvocation;
import static net.codecadenza.eclipse.shared.Constants.PACK_JAVA_UTIL;

import java.util.HashSet;
import java.util.Set;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.integration.method.AbstractIntegrationMethodGenerator;
import net.codecadenza.eclipse.model.integration.RMIIntegrationMethod;
import net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration;

/**
 * <p>
 * Base class for RMI integration method generators
 * </p>
 * <p>
 * Copyright 2016 (C) Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class BasicRMIMethodGenerator extends AbstractIntegrationMethodGenerator {
	protected final RMIIntegrationMethod rmiMethod;

	/**
	 * Constructor
	 * @param rmiMethod
	 * @param parentGenerator
	 */
	public BasicRMIMethodGenerator(RMIIntegrationMethod rmiMethod, AbstractJavaSourceGenerator parentGenerator) {
		super(rmiMethod, parentGenerator);

		this.rmiMethod = rmiMethod;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.integration.method.AbstractIntegrationMethodGenerator#createMethod()
	 */
	@Override
	public String createMethod() {
		final var b = new StringBuilder();
		b.append("@Override\n");
		b.append(parentGenerator.getAnnotationForGeneratedElement());
		b.append(getMethodSignature(true, true, true));
		b.append("\n{\n");
		b.append(createMethodBody());
		b.append("}\n\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.integration.method.AbstractIntegrationMethodGenerator#createMethodLogic()
	 */
	@Override
	public String createMethodLogic() {
		final var b = new StringBuilder();
		final String facadeFragment = createFacadeFragment();

		if (facadeFragment.isEmpty()) {
			if (!rmiMethod.getReturnType().isVoid())
				b.append("return ");

			b.append(createServiceMethodInvocation(rmiMethod, getServiceName()));
		}
		else {
			b.append(facadeFragment);

			if (!rmiMethod.getReturnType().isVoid())
				b.append("return " + getReturnObjectName() + ";\n");
		}

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.integration.method.AbstractIntegrationMethodGenerator#getImports()
	 */
	@Override
	public Set<String> getImports() {
		final var imports = new HashSet<>(super.getImports());
		imports.add("import net.codecadenza.runtime.transport.*;");

		return imports;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.integration.method.AbstractIntegrationMethodGenerator#getInterfaceImports()
	 */
	@Override
	public Set<String> getInterfaceImports() {
		final var imports = new HashSet<>(super.getInterfaceImports());
		imports.add("import net.codecadenza.runtime.transport.*;");

		if (rmiMethod.getReturnTypeModifier() != JavaTypeModifierEnumeration.NONE)
			imports.add("import " + PACK_JAVA_UTIL + ".*;");

		return imports;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.integration.method.AbstractIntegrationMethodGenerator#addTransaction()
	 */
	@Override
	public boolean addTransaction() {
		return false;
	}

}

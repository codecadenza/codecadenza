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
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.java.MethodParameter;

/**
 * <p>
 * Generator for boundary methods that move a target reference of a unidirectional one-to-many association
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ChangeAssociationBoundaryMethodGenerator extends BasicBoundaryMethodGenerator {
	/**
	 * Constructor
	 * @param method
	 * @param utility
	 * @param parentGenerator
	 */
	public ChangeAssociationBoundaryMethodGenerator(BoundaryMethod method, IMethodGeneratorUtility utility,
			AbstractJavaSourceGenerator parentGenerator) {
		super(method, utility, parentGenerator);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.boundary.method.BasicBoundaryMethodGenerator#createMethodBody()
	 */
	@Override
	public String createMethodBody() {
		final var b = new StringBuilder();
		final OneToManyAssociation otm = (OneToManyAssociation) method.getAssociation();
		final String sourceDomainObjectName = otm.getDomainObject().getName();
		final String referencedDomainObjectName = otm.getTarget().getName();
		final String sourceObjectName = "from" + sourceDomainObjectName;
		final String targetObjectName = "to" + sourceDomainObjectName;
		final String objectName = otm.getTarget().getLowerCaseName();
		final String repositoryMethodName = method.getServiceMethod().getName();
		boolean isFirstParam = true;
		MethodParameter targetParameter = null;
		MethodParameter sourceParameter = null;

		for (final MethodParameter p : method.getMethodParameters()) {
			if (!isFirstParam) {
				sourceParameter = p;
				break;
			}

			isFirstParam = false;
			targetParameter = p;
		}

		if (targetParameter == null || sourceParameter == null) {
			final var msg = "The method '" + method.getName() + "' could not be created as the expected parameters are missing!";

			throw new IllegalStateException(msg);
		}

		b.append("final TypedQuery<" + sourceDomainObjectName + "> query = ");

		if (project.isJavaSEApplication())
			b.append("em");
		else
			b.append(getRepositoryName() + "getEntityManager()");

		b.append(".createNamedQuery(");
		b.append(sourceDomainObjectName + ".NQ_GET_BY_" + otm.getName().toUpperCase() + ", " + sourceDomainObjectName + ".class);\n");
		b.append("query.setParameter(\"" + otm.getDomainObject().getPKAttribute().getName() + "\", ");
		b.append(targetParameter.getName() + ");\n\n");
		b.append("// Ignore cases where the object is referenced more than once\n");
		b.append("final " + sourceDomainObjectName + " " + sourceObjectName);
		b.append(" = query.getResultList().stream().findFirst().orElse(null);\n");
		b.append("final " + sourceDomainObjectName + " " + targetObjectName + " = ");
		b.append(getRepositoryName() + repositoryMethodName + "(" + sourceDomainObjectName);
		b.append(".class, " + sourceParameter.getName() + ");\n\n");
		b.append("final " + referencedDomainObjectName + " " + objectName + " = ");
		b.append(getRepositoryName() + repositoryMethodName + "(" + referencedDomainObjectName);
		b.append(".class, " + targetParameter.getName() + ");\n\n");
		b.append("if(" + sourceObjectName + " != null)\n");
		b.append(sourceObjectName + "." + otm.getGetterName() + ".remove(" + objectName + ");\n\n");
		b.append(targetObjectName + "." + otm.getGetterName() + ".add(" + objectName + ");\n");

		if (addTransactionManagement)
			b.append("\ntr.commit();\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.boundary.method.BasicBoundaryMethodGenerator#getImports()
	 */
	@Override
	public Set<String> getImports() {
		final Set<String> imports = new HashSet<>(super.getImports());
		imports.add("import jakarta.persistence.*;");
		imports.add("import " + method.getAssociation().getTarget().getNamespace().toString() + ".*;");

		return imports;
	}

}

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
package net.codecadenza.runtime.webclient.vaadin.provider.data;

import com.vaadin.flow.data.provider.AbstractBackEndDataProvider;
import com.vaadin.flow.data.provider.Query;
import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * A data provider that loads items from a back-end. In contrast to a {@link AbstractBackEndDataProvider} this data provider only
 * needs one callback function in order to provide the items! This is necessary as CodeCadenza doesn't create corresponding count
 * methods for simple look-up operations.
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type that is returned by the data provider
 */
public class BackEndDataProvider<T> extends AbstractBackEndDataProvider<T, String> {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private static final long serialVersionUID = -4944794744604525065L;

	private final FetchCallback<T> fetchCallback;
	private transient List<T> items;

	/**
	 * @param fetchCallback
	 * @return an instance of this data provider using the given callback
	 */
	public static <T> BackEndDataProvider<T> fromCallback(FetchCallback<T> fetchCallback) {
		return new BackEndDataProvider<>(fetchCallback);
	}

	/**
	 * Constructor
	 * @param fetchCallback
	 */
	public BackEndDataProvider(FetchCallback<T> fetchCallback) {
		this.fetchCallback = fetchCallback;
	}

	/**
	 * @return the callback that is responsible for fetching data
	 */
	public FetchCallback<T> getFetchCallback() {
		return fetchCallback;
	}

	/*
	 * (non-Javadoc)
	 * @see com.vaadin.data.provider.AbstractBackEndDataProvider#fetchFromBackEnd(com.vaadin.data.provider.Query)
	 */
	@Override
	protected Stream<T> fetchFromBackEnd(Query<T, String> query) {
		// Return an empty stream if no data is available!
		if (items == null)
			return new ArrayList<T>().stream();

		// The method getOffset() must be invoked in order to avoid an IllegalStateException!
		query.getOffset();

		// The list of items should have already been loaded by the method sizeInBackEnd()
		return items.stream().limit(query.getLimit());
	}

	/*
	 * (non-Javadoc)
	 * @see com.vaadin.data.provider.AbstractBackEndDataProvider#sizeInBackEnd(com.vaadin.data.provider.Query)
	 */
	@Override
	protected int sizeInBackEnd(Query<T, String> query) {
		try {
			// Fetch the items from the back-end and save the list in order to reuse it in the method fetchFromBackEnd()
			items = fetchCallback.fetchData();

			return items.size();
		}
		catch (final Exception e) {
			logger.error("Error while fetching data!", e);
		}

		return 0;
	}

}

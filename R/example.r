library('pricesensitivity')

pricing_stats <- pricesensitivity::generate_pricing_stats(pricesensitivity::example_data, 
                                                          too_expensive = 'too_expensive', 
                                                          bargain ='bargain',
                                                          too_cheap = 'too_cheap', getting_expensive = 'getting_expensive')

pricesensitivity::visualize_pricing_stats(pricing_stats$data, 
                                          pricing_stats$metadata, 
                                          title = 'Product pricing',
                                          currency = 'dollars')